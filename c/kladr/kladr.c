/* @(#)kladr.c
 */

#include <gtk/gtk.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef struct {
  sqlite3 *connection;
  GtkTreeModel *model;
  GtkTreeView *view;
} ModelAndConnection;

typedef struct {
  ModelAndConnection *model_and_connection;
  GtkTreeIter *parent;
} TreeParent;

gboolean on_window_close(GtkWidget *window, GdkEvent *event, gpointer user_data)
{
  ModelAndConnection *data = (ModelAndConnection *)user_data;
  sqlite3_close(data->connection);
  free(data);
  gtk_main_quit();
  return FALSE;
};

GtkTreeIter *append_value(GtkTreeStore *store, GtkTreeIter *parent, int val0, gchar *val1, gchar *val2)
{
  GtkTreeIter set;
  gtk_tree_store_append(store, &set, parent);
  gtk_tree_store_set(store, &set, 0, val0, 1, val1, 2, val2, -1);
  return gtk_tree_iter_copy(&set);
}

gpointer child_builder_thread(gpointer user_data)
{
  TreeParent *data = (TreeParent *)user_data;
  gdk_threads_enter();
  gtk_widget_freeze_child_notify(GTK_WIDGET(data->model_and_connection->view));
  
  GValue rvalue;
  gtk_tree_model_get_value(GTK_TREE_MODEL(data->model_and_connection->model),
                           data->parent,
                           0, &rvalue);

  char *childs = "select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join kladr_hierarchy h on h.child = k.id where h.parent = ? order by t.name, k.name";
  sqlite3_stmt * chstmt;
  if (SQLITE_OK != sqlite3_prepare(data->model_and_connection->connection, childs, -1, &chstmt, NULL)) {
    g_printerr("Can not create statement %s", childs);
    return NULL;
  }

  sqlite3_bind_int(chstmt, 1, g_value_get_int(&rvalue));
  while (SQLITE_ROW == sqlite3_step(chstmt)) {
    GtkTreeIter *child = append_value(GTK_TREE_STORE(data->model_and_connection->model),
                                      data->parent,
                                      sqlite3_column_int(chstmt, 0),
                                      g_strdup(sqlite3_column_text(chstmt, 1)),
                                      g_strdup(sqlite3_column_text(chstmt, 2)));
  }
  sqlite3_finalize(chstmt);
    
  gtk_widget_thaw_child_notify(GTK_WIDGET(data->model_and_connection->view));
  gdk_threads_leave();
  g_free(user_data);
  return NULL;
};

  

void build_children(GtkTreeView *view, GtkTreeIter *iter, GtkTreePath *path, gpointer user_data)
{
  TreeParent *data = g_malloc(sizeof(TreeParent));
  data->parent = iter;
  data->model_and_connection = (ModelAndConnection *)user_data;
  if (NULL == g_thread_create(&child_builder_thread, data, FALSE, NULL)) {
    g_printerr("Can not create thread");
  }
  return;
};



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
                                        g_strdup(sqlite3_column_text(root_stmt, 1)),
                                        g_strdup(sqlite3_column_text(root_stmt, 2)));
    //gtk_widget_thaw_child_notify(GTK_WIDGET(data->view));
    gdk_threads_leave();
    
    sqlite3_bind_int(child_stmt, 1, root_id);
    while(SQLITE_ROW == sqlite3_step(child_stmt)) {
      gdk_threads_enter();
      //gtk_widget_freeze_child_notify(GTK_WIDGET(data->view));
      GtkTreeIter *child = append_value(GTK_TREE_STORE(data->model), parent,
                                        sqlite3_column_int(child_stmt, 0),
                                        g_strdup(sqlite3_column_text(child_stmt, 1)),
                                        g_strdup(sqlite3_column_text(child_stmt, 2)));
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


void build_and_run(char *filename)
{
  sqlite3 *connection;
  if (sqlite3_open(filename, /*@out@*/ &connection) != SQLITE_OK) {
    printf("Can not open file %s", filename);
    return;
  }
  g_print("Connection set");
  GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  GtkWidget *sw = gtk_scrolled_window_new(NULL, NULL);
  GtkWidget *view = gtk_tree_view_new();
  gtk_container_add(GTK_CONTAINER(window), GTK_WIDGET(sw));
  gtk_container_add(GTK_CONTAINER(sw), GTK_WIDGET(view));
  GtkTreeModel *model = GTK_TREE_MODEL(gtk_tree_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING));
  gtk_tree_view_set_model(GTK_TREE_VIEW(view), model);
  GtkTreeViewColumn *col1 = gtk_tree_view_column_new();
  GtkTreeViewColumn *col2 = gtk_tree_view_column_new();
  GtkCellRenderer *ren1 = gtk_cell_renderer_text_new();
  GtkCellRenderer *ren2 = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(col1, ren1, TRUE);
  gtk_tree_view_column_add_attribute(col1, ren1, "text", 1);
  gtk_tree_view_column_pack_start(col2, ren2, TRUE);
  gtk_tree_view_column_add_attribute(col2, ren2, "text", 2);
  gtk_tree_view_append_column(GTK_TREE_VIEW(view), col1);
  gtk_tree_view_append_column(GTK_TREE_VIEW(view), col2);

  ModelAndConnection *data = malloc(sizeof(ModelAndConnection));
  if (NULL == data) {
    printf("Can not allocate memory");
  }
  data->connection = connection;
  data->model = model;
  data->view = GTK_TREE_VIEW(view);

  g_signal_connect(G_OBJECT(view), "test-expand-row", G_CALLBACK(build_children), data);
  g_signal_connect(G_OBJECT(window), "delete-event", G_CALLBACK(on_window_close), data);
  gtk_widget_show_all(GTK_WIDGET(window));

  run_building_root(data);

};

int main(int argc, char **argv) {
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
