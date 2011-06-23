/* @(#)kladr.c
 */

#include <gtk/gtk.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/**
   Структура педерается во все обработчики нажатий кнопок и так далее.
   Хранит все основные элементы
*/
typedef struct {
  sqlite3 *connection;
  GtkTreeModel *model;
  GtkTreeView *view;
  
  /**
     Список элементов которые уже раскрыли.
     @todo
     Здаействовать это список,
     подумать как сделать так чтобы при совершении rollback и перестроении модели
     этот список оставался актуальным
  */
  GArray *expanded; 
                       
} ModelAndConnection;

typedef struct {
  ModelAndConnection *model_and_connection;
  GtkTreeIter *parent;
} TreeParent;


/**
   Структура для запоминания элементов которые выбираются запросом
*/
typedef struct {
  ///Очередь результатов запроса
  GQueue *queue;
  ///Готовый стейтмент с забинденными параметрами
  sqlite3_stmt *statement;
} QueueAndStatement;

gboolean on_window_close(GtkWidget *window ///The window executed signal
                         , GdkEvent *event ///The signal
                         , gpointer user_data /**
                                                 The universal answer.
                                                 Must be cumputed by formula
                                                 \f[
                                                 |I_2|=\left| \int_{0}^T \psi(t) 
                                                 \left\{ 
                                                 u(a,t)-
                                                 \int_{\gamma(t)}^a 
                                                 \frac{d\theta}{k(\theta,t)}
                                                 \int_{a}^\theta c(\xi)u_t(\xi,t)\,d\xi
                                                 \right\} dt
                                                 \right|
                                                 \f]
                                              */
                                                 
                         )
{
  ModelAndConnection *data = (ModelAndConnection *)user_data;
  sqlite3_close(data->connection);
  free(data);
  gtk_main_quit();
  return FALSE;
}

GtkTreeIter *append_value(GtkTreeStore *store, GtkTreeIter *parent, int val0, gchar *val1, gchar *val2)
{
  GtkTreeIter set;
  gtk_tree_store_append(store, &set, parent);
  gtk_tree_store_set(store, &set, 0, val0, 1, val1, 2, val2, -1);
  return gtk_tree_iter_copy(&set);
}

/**
   \brief Построитель дочерних элементов.
   
   Функция выполняется в отдельном потоке, читает из базы дочерние элементы дочерних элементов раскрытого элемента модели.
   \bug
   Если сделать коммит или роллбек пока выполяется заполнение дочерних элементов программа может сегфолтнутся, при этом валятся Gtk ассершены по поводу того что итератор мол не рабочий

   \todo
   аналогично с root_builder_thread
   
   \retrun Всегда должен возвращать NULL
*/
   
gpointer child_builder_thread(gpointer user_data)
  
{
  gdk_threads_enter();
  TreeParent *data = (TreeParent *)user_data;
  GtkTreeIter first_child;
  gboolean proceed = gtk_tree_model_iter_children(GTK_TREE_MODEL(data->model_and_connection->model),
                                                  &first_child,
                                                  data->parent);
  gdk_threads_leave();
  if (! proceed) {
    gdk_threads_enter();
    gtk_tree_iter_free(data->parent);
    g_free(data);
    gdk_threads_leave();
    return NULL;
  }
  
  char *childs = "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join kladr_hierarchy h on h.child = k.id where h.parent = ? order by t.name, k.name";
  sqlite3_stmt * chstmt;
  if (SQLITE_OK != sqlite3_prepare(data->model_and_connection->connection, childs, -1, &chstmt, NULL)) {
    gdk_threads_enter();
    gtk_tree_iter_free(data->parent);
    g_free(data);
    gdk_threads_leave();
    g_printerr("Can not create statement %s", childs);
    return NULL;
  }
  gboolean cont = FALSE;
  do {
    gdk_threads_enter();
    int parent_id;
    gtk_tree_model_get(GTK_TREE_MODEL(data->model_and_connection->model),
                       &first_child,
                       0, &parent_id, -1);
    gdk_threads_leave();
    sqlite3_bind_int(chstmt, 1, parent_id);
    while (SQLITE_ROW == sqlite3_step(chstmt)) {
      gdk_threads_enter();
      int val0 = sqlite3_column_int(chstmt, 0);
      gchar *val1 = g_strdup((gchar*)sqlite3_column_text(chstmt, 1));
      gchar *val2 = g_strdup((gchar*)sqlite3_column_text(chstmt, 2));
      GtkTreeIter *child = append_value(GTK_TREE_STORE(data->model_and_connection->model),
                                        &first_child,
                                        val0,
                                        val1,
                                        val2);
      g_free(val1); g_free(val2);
      gtk_tree_iter_free(child);
      gdk_threads_leave();
    }
    sqlite3_reset(chstmt);
    sqlite3_clear_bindings(chstmt);
    gdk_threads_enter();
    cont = gtk_tree_model_iter_next(GTK_TREE_MODEL(data->model_and_connection->model),
                                             &first_child);
    gdk_threads_leave();
  } while (TRUE == cont);
    
  gdk_threads_enter();
  gtk_tree_iter_free(data->parent);
  g_free(user_data);
  gdk_threads_leave();
  return NULL;
};

  

void build_children(GtkTreeView *view, GtkTreeIter *iter, GtkTreePath *path, gpointer user_data)
{
  TreeParent *data = g_malloc(sizeof(TreeParent));
  data->parent = gtk_tree_iter_copy(iter);
  data->model_and_connection = (ModelAndConnection *)user_data;
  /*int parent_id;
  gtk_tree_model_get(data->model_and_connection->model,
                     iter, 0, &parent_id, -1);
                     if */
  if (NULL == g_thread_create(&child_builder_thread, data, FALSE, NULL)) {
    g_printerr("Can not create thread");
  }
  return;
};


/**
   \brief Функия работающая в отдельном потоке. Заполняет корневые элементы

   \todo
   Переделать ее с использованием \c g_idle_add чтобы интерфейс не тормозил когда добавляются элементы
*/
gpointer root_builder_thread(gpointer user_data)
{
  ModelAndConnection *data = (ModelAndConnection *)user_data;
  char *select_roots = "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id where exists(select h.* from kladr_hierarchy h where h.parent = k.id) and not exists(select hh.* from kladr_hierarchy hh where hh.child = k.id) order by t.name, k.name";
  char *select_childs = "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join kladr_hierarchy h on h.child = k.id where h.parent = ? order by t.name, k.name";
  sqlite3_stmt *root_stmt, *child_stmt;
  if (SQLITE_OK != sqlite3_prepare(data->connection, select_roots, -1, &root_stmt, NULL)) {
    g_printerr ("can not prepare statement %s", select_roots);
    return NULL;
  }
  if (SQLITE_OK != sqlite3_prepare(data->connection, select_childs, -1, &child_stmt, NULL)) {
    g_printerr("can not prepare %s", select_childs);
    sqlite3_finalize(root_stmt);
    return NULL;
  }
  while (SQLITE_ROW == sqlite3_step(root_stmt)) {
    gdk_threads_enter();
    int root_id = sqlite3_column_int(root_stmt, 0);
    //gtk_widget_freeze_child_notify(GTK_WIDGET(data->view));
    GtkTreeIter *parent =  append_value(GTK_TREE_STORE(data->model), NULL,
                                        root_id,
                                        g_strdup((gchar*)sqlite3_column_text(root_stmt, 1)),
                                        g_strdup((gchar*)sqlite3_column_text(root_stmt, 2)));
    //gtk_widget_thaw_child_notify(GTK_WIDGET(data->view));
    gdk_threads_leave();
    
    sqlite3_bind_int(child_stmt, 1, root_id);
    while(SQLITE_ROW == sqlite3_step(child_stmt)) {
      gdk_threads_enter();
      //gtk_widget_freeze_child_notify(GTK_WIDGET(data->view));
      GtkTreeIter *child = append_value(GTK_TREE_STORE(data->model), parent,
                                        sqlite3_column_int(child_stmt, 0),
                                        g_strdup((gchar*)sqlite3_column_text(child_stmt, 1)),
                                        g_strdup((gchar*)sqlite3_column_text(child_stmt, 2)));
      //gtk_widget_thaw_child_notify(GTK_WIDGET(data->view));
      gtk_tree_iter_free(child);
      gdk_flush();
      gdk_threads_leave();
    };
    sqlite3_reset(child_stmt);
    sqlite3_clear_bindings(child_stmt);
    
    gdk_threads_enter();
    gtk_tree_iter_free(parent);
    gdk_threads_leave();
   }
  sqlite3_finalize(root_stmt);
  sqlite3_finalize(child_stmt);
  return NULL;
};

void run_building_root(gpointer user_data)
{
  if (NULL == g_thread_create(&root_builder_thread, user_data, FALSE, NULL)) {
    g_printerr("Can not create thread");
  }
};

void construct_and_pack(GtkWidget **window ///Сюда вернет указтель на новое окно
                        , GtkWidget **view ///Суда вернет указатель на GtkTreeView
                        , GtkTreeModel **model ///Суда вернет указатель на GtkTreeModel которую он связет с вьюхой
                        , GtkWidget **delete_button ///Сюда вернет на GtkButton при нажатии на которую должно удалятся дерево
                        , GtkWidget **delete_same_button ///Сюда вернет указатель на GtkButton по нажатии на котору должны удалится элементы того же типа из уровня выделенного элемента
                        , GtkWidget **commit_button ///вернет уазатель накнопку коммита
                        , GtkWidget **rollback_button ///Вернет указатель на кнопку Rollback
                        )
/**
   Создает виджеты, пакует, настраивает их. Обработчики событий к виджетам не привязывает.
*/
{
  g_assert(NULL != window);
  g_assert(NULL != view);
  g_assert(NULL != delete_button);
  g_assert(NULL != delete_same_button);
  GtkWidget *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(win), "Kladr tree");
  gtk_window_resize(GTK_WINDOW(win), 600, 400);
  GtkWidget *vbox = GTK_WIDGET(gtk_vbox_new(FALSE, 0));
  gtk_container_add(GTK_CONTAINER(win), GTK_WIDGET(vbox));
  GtkWidget *scrolled_window = GTK_WIDGET(gtk_scrolled_window_new(NULL, NULL));
  gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(scrolled_window), TRUE, TRUE, 0);
  GtkWidget *tree_view = GTK_WIDGET(gtk_tree_view_new());
  gtk_container_add(GTK_CONTAINER(scrolled_window), tree_view);
  GtkWidget *hbox = GTK_WIDGET(gtk_hbox_new(FALSE, 0));
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
  GtkWidget *del_button = GTK_WIDGET(gtk_button_new());
  gtk_button_set_label(GTK_BUTTON(del_button), "Delete subtree");
  GtkWidget *del_same_button = GTK_WIDGET(gtk_button_new());
  gtk_button_set_label(GTK_BUTTON(del_same_button), "Delete same from this level");
  GtkWidget *commit = GTK_WIDGET(gtk_button_new());
  gtk_button_set_label(GTK_BUTTON(commit), "Commit");
  GtkWidget *rollback = GTK_WIDGET(gtk_button_new());
  gtk_button_set_label(GTK_BUTTON(rollback), "Rollback");
  gtk_box_pack_start(GTK_BOX(hbox), del_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), del_same_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), commit, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), rollback, FALSE, FALSE, 0);

  GtkTreeModel *m = GTK_TREE_MODEL(gtk_tree_store_new(3, G_TYPE_UINT, G_TYPE_STRING, G_TYPE_STRING));
  gtk_tree_view_set_model(GTK_TREE_VIEW(tree_view), m);

  GtkTreeViewColumn *col1 = gtk_tree_view_column_new();
  GtkTreeViewColumn *col2 = gtk_tree_view_column_new();
  GtkCellRenderer *ren1 = gtk_cell_renderer_text_new();
  GtkCellRenderer *ren2 = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(col1, ren1, TRUE);
  gtk_tree_view_column_add_attribute(col1, ren1, "text", 1);
  gtk_tree_view_column_pack_start(col2, ren2, TRUE);
  gtk_tree_view_column_add_attribute(col2, ren2, "text", 2);
  gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), col1);
  gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), col2);

  (*window) = win;
  (*view) = tree_view;
  (*model) = m;
  (*delete_button) = del_button;
  (*delete_same_button) = del_same_button;
  (*commit_button) = commit;
  (*rollback_button) = rollback;
  return;
}

void on_delete_subtree_clicked(GtkButton *button, gpointer user_data) {
  ModelAndConnection *data = (ModelAndConnection*)user_data;
  GtkTreeSelection *selection = gtk_tree_view_get_selection(data->view);
  GtkTreeIter selected_iter;
  gboolean selected = gtk_tree_selection_get_selected(selection, NULL, &selected_iter);
  if (TRUE == selected) {
    sqlite3_stmt *get_childs;
    int prep = sqlite3_prepare(data->connection, "select child from kladr_hierarchy where parent = ?", -1, &get_childs, NULL);
    if (SQLITE_OK != prep) {
      g_printerr("Can not prepare statement");
      return;
    }
    sqlite3_stmt *remove_h;
    prep = sqlite3_prepare(data->connection, "delete from kladr_hierarchy where parent = ?", -1, &remove_h, NULL);
    if (SQLITE_OK != prep) {
      sqlite3_finalize(get_childs);
      g_printerr("Can not prepare statement");
      return;
    }

    sqlite3_stmt *begin;        /* statement to start transaction */
    prep = sqlite3_prepare(data->connection, "begin", -1, &begin, NULL);
    if (SQLITE_OK != prep) {
      sqlite3_finalize(get_childs);
      sqlite3_finalize(remove_h);
      g_printerr("Can not prepare 'bebin' statement");
      return;
    }

    sqlite3_step(begin);        /* ensure transaction started */
    
    guint parent_id;
    gtk_tree_model_get(data->model, &selected_iter, 0, &parent_id, -1);
    gtk_tree_store_remove(GTK_TREE_STORE(data->model), &selected_iter);

    guint *first_parent = g_malloc(sizeof(guint));
    (*first_parent) = parent_id;
    GQueue *childs = g_queue_new();
    g_queue_push_tail(childs, first_parent);
    
    //now enter deleting loop
    do {
      
      //first collect all the childs
      void foreach_get_childs(gpointer data, gpointer user_data) /* collecting callback */
      {
        guint *pid = (guint*)data;
        QueueAndStatement *queue_and_statement = (QueueAndStatement*)user_data;
        guint *cid;
        sqlite3_bind_int(queue_and_statement->statement, 1, *pid);
        while (SQLITE_ROW == sqlite3_step(queue_and_statement->statement)) {
          cid = g_malloc(sizeof(guint));
          *cid = sqlite3_column_int(queue_and_statement->statement, 0);
          g_queue_push_tail(queue_and_statement->queue, cid);
        }
        sqlite3_reset(queue_and_statement->statement);
        sqlite3_clear_bindings(queue_and_statement->statement);
        return;
      }
      QueueAndStatement *pass_data = g_malloc(sizeof(QueueAndStatement));
      pass_data->queue = g_queue_new(); /* here we will have all the childs of our `childs` */
      pass_data->statement = get_childs;
      g_queue_foreach(childs, &foreach_get_childs, pass_data);

      //now remove from the database records
      void foreach_execute_remove(gpointer data, gpointer user_data) /* remover callback */
      {
        guint *pid = (guint*)data;
        sqlite3_stmt *remstat = (sqlite3_stmt*)user_data;
        sqlite3_bind_int(remstat, 1, *pid);
        sqlite3_step(remstat);
        sqlite3_reset(remstat);
        sqlite3_clear_bindings(remstat);
        return;
      }
      g_queue_foreach(childs, &foreach_execute_remove, remove_h);

      void foreach_g_free(gpointer data, gpointer user_data) /* free all guint's malloc'ed from collecting callback */
      {
        g_free(data);
        return;
      }
      g_queue_foreach(childs, &foreach_g_free, NULL);
      g_queue_free(childs);     /* free structure memory */
    
      childs = pass_data->queue; /* now bring up childs to parents */
      g_free(pass_data);         /* free dinamically allocated structure */
    
    
    } while (childs->length >0); /* until we can collect any childs */

    sqlite3_finalize(remove_h);
    sqlite3_finalize(get_childs);
    sqlite3_finalize(begin);
    
  }
    
}

void on_commit_clicked(GtkButton *button, gpointer *user_data) {
  ModelAndConnection *data = (ModelAndConnection*) user_data;
  sqlite3_stmt *commit;
  int ret = sqlite3_prepare(data->connection, "commit", -1, &commit, NULL);
  if (SQLITE_OK != ret) {
    g_printerr("Can not prepare 'commit' statement");
    return;
  }
  sqlite3_step(commit);         /* do commit */
  sqlite3_finalize(commit);
  return;
};

void on_rollback_clicked(GtkButton *button, gpointer *user_data) {
  ModelAndConnection *data = (ModelAndConnection *) user_data;
  sqlite3_stmt *rollback;
  int ret = sqlite3_prepare(data->connection, "rollback", -1, &rollback, NULL);
  if (SQLITE_OK != ret) {
    g_printerr("Can not prepare statement 'rollback'");
    return;
  }

  sqlite3_step(rollback);
  sqlite3_finalize(rollback);
  data->model = GTK_TREE_MODEL(gtk_tree_store_new(3, G_TYPE_UINT, G_TYPE_STRING, G_TYPE_STRING));
  gtk_tree_view_set_model(data->view, GTK_TREE_MODEL(data->model));
  run_building_root(user_data);
};


void build_and_run(char *filename /** Файло для открытия скулайтиком */
                   )
{
  sqlite3 *connection;
  if (sqlite3_open(filename, /*@out@*/ &connection) != SQLITE_OK) {
    printf("Can not open file %s", filename);
    return;
  }
  g_print("Connection set");
  
  GtkWidget *window, *view, *delete_button, *delete_same_button, *commit_button, *rollback_button;
  GtkTreeModel *model;
  construct_and_pack(&window, &view, &model, &delete_button, &delete_same_button, &commit_button, &rollback_button);

  ModelAndConnection *data = malloc(sizeof(ModelAndConnection));
  if (NULL == data) {
    printf("Can not allocate memory");
  }
  
  data->connection = connection;
  data->model = model;
  data->view = GTK_TREE_VIEW(view);
  data->expanded = g_array_new(TRUE, FALSE, sizeof(guint));

  g_signal_connect(G_OBJECT(view), "row-expanded", G_CALLBACK(build_children), data);
  g_signal_connect(G_OBJECT(window), "delete-event", G_CALLBACK(on_window_close), data);
  g_signal_connect(G_OBJECT(delete_button), "clicked",
                   G_CALLBACK(on_delete_subtree_clicked), data);
  g_signal_connect(G_OBJECT(commit_button), "clicked",
                   G_CALLBACK(on_commit_clicked), data);
  g_signal_connect(G_OBJECT(rollback_button), "clicked",
                   G_CALLBACK(on_rollback_clicked), data);

  
  gtk_window_resize(GTK_WINDOW(window), 500, 400);
  
  
  gtk_widget_show_all(GTK_WIDGET(window));

  run_building_root(data);

  

};

int main(int argc /** count of bla bla */,
         char **argv /** the blah blah */
         )
{
  /**
     Гллагне функцие что делает все
     
     \arg \c argc Глаге количество аргументов
     \arg \c argv Массив массивов символов
  */
  if (argc == 2) {
    g_thread_init(NULL);
    gdk_threads_init();
    gtk_init(&argc, &argv);

    build_and_run(argv[1]);
    gdk_threads_enter();
    gtk_main();
    gdk_threads_leave();
  }
  return 0;
}
