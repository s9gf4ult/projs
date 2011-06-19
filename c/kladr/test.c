
#include <gtk/gtk.h>

int main(int argc, char **argv) {
  gtk_init(&argc, &argv);
  
  GtkTreeStore *model = gtk_tree_store_new(3, G_TYPE_UINT, G_TYPE_STRING, G_TYPE_STRING);
  GtkTreeIter iter;

  for(guint i=0; i<=45; i++) {
    gtk_tree_store_append(model, &iter, NULL);
    gchar *val1 = g_strdup_printf("There is the %d parrots on the wall", i);
    gchar *val2 = g_strdup_printf("Eat over %d this franch batoons !!!", i*200);
    gtk_tree_store_set(model, &iter, 0, i, 1, val1, 2, val2, -1);
    g_free(val1); g_free(val2);
  }

  gtk_tree_model_get_iter_first(GTK_TREE_MODEL(model), &iter);
  do {
    guint val0;
    gchar *val1, *val2;
    gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 0, &val0, 1, &val1, 2, &val2, -1);
    g_print("(%d, %s, %s)\n", val0, val1, val2);
    g_free(val1); g_free(val2);
  } while (gtk_tree_model_iter_next(GTK_TREE_MODEL(model), &iter) == TRUE);
  
  
  return 0;
}
