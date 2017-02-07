/**
 *  * section: Tree
 *   * synopsis: Navigates a tree to print element names
 *    * purpose: Parse a file to a tree, use xmlDocGetRootElement() to
 *     *          get the root element, then walk the document and print
 *      *          all the element name in document order.
 *       * usage: tree1 filename_or_URL
 *        * test: tree1 test2.xml > tree1.tmp && diff tree1.tmp
 * $(srcdir)/tree1.res
 *         * author: Dodji Seketeli
 *          * copy: see Copyright for the status of this software.
 *           */
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <stdio.h>
#include <string.h>

#ifdef LIBXML_TREE_ENABLED

/*
 *  *To compile this file using gcc you can type
 *   *gcc `xml2-config --cflags --libs` -o xmlexample libxml2-example.c
 *    */

/**
 *  * print_element_names:
 *   * @a_node: the initial xml node to consider.
 *    *
 *     * Prints the names of the all the xml elements
 *      * that are siblings or children of a given xml node.
 *       */

int verbose = 1;

#define mprintf                                                                \
  if (verbose)                                                                 \
  printf

#define get_value(cur_node, _name, _l)                                         \
  do {                                                                         \
    if (strcmp(#_name, cur_node->name) == 0) {                                 \
      mprintf("\tnode type: Element, %s: %s\n", #_name,                        \
              cur_node->children->content);                                    \
      _l->_name = atoi(cur_node->children->content);                           \
    }                                                                          \
  } while (0)

#define get_value_name(cur_node, _name, _l)                                    \
  do {                                                                         \
    if (strcmp(#_name, cur_node->name) == 0) {                                 \
      mprintf("\tnode type: Element, %s: %s\n", #_name,                        \
              cur_node->children->content);                                    \
      _l->_name = calloc(strlen(cur_node->children->content) + 1, 1);          \
      memcpy(_l->_name, cur_node->children->content,                           \
             strlen(cur_node->children->content) + 1);                         \
    }                                                                          \
  } while (0)

typedef struct {
  char *name;
  int failures;
  int tests;
  int errors;
  int success;
  int skipped;
} sctk_leaf_t;

static void print_element_names_leaf(xmlNode *a_node, int *table_size,
                                     sctk_leaf_t **table) {
  xmlNode *cur_node = NULL;
  sctk_leaf_t *l;
  (*table_size)++;
  *table = realloc(*table, *table_size * sizeof(sctk_leaf_t));

  l = &((*table)[(*table_size) - 1]);

  for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
    if (cur_node->type == XML_ELEMENT_NODE) {
      get_value_name(cur_node, name, l);
      get_value(cur_node, failures, l);
      get_value(cur_node, tests, l);
      get_value(cur_node, errors, l);
      get_value(cur_node, skipped, l);
      get_value(cur_node, success, l);
    }
  }
}

static void print_element_names(xmlNode *a_node, int *table_size,
                                sctk_leaf_t **table) {
  xmlNode *cur_node = NULL;

  for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
    if ((cur_node->type == XML_ELEMENT_NODE) &&
        (strcmp("testdir", cur_node->name) == 0)) {
      print_element_names_leaf(cur_node->children, table_size, table);
    } else {
      print_element_names(cur_node->children, table_size, table);
    }
  }
}

static int validate_results(sctk_leaf_t *table, int table_size,
                            sctk_leaf_t *new_table, int new_table_size) {

  int i, j;
  int errors = 0;

  mprintf("Sizes %d %d\n", table_size, new_table_size);

  for (i = 0; i < table_size; i++) {
    for (j = 0; j < new_table_size; j++) {
      if (strcmp(table[i].name, new_table[j].name) == 0) {
        mprintf("Test %s\n", table[i].name);
        if (table[i].tests == new_table[j].tests) {
          if (table[i].success > new_table[j].success) {
            mprintf("\tRegression on %s %d -> %d\n", table[i].name,
                    table[i].success, new_table[j].success);
            errors++;
          }
        } else {
          if (table[i].tests > new_table[j].tests) {
            if ((((double)table[i].success) / ((double)table[i].tests)) >
                (((double)new_table[j].success) /
                 ((double)new_table[j].tests))) {
              mprintf("\tRegression on %s %d -> %d\n", table[i].name,
                      table[i].success, new_table[j].success);
              errors++;
            }
          } else {
            if (table[i].tests < new_table[j].tests) {
              if (table[i].success > new_table[j].success) {
                mprintf("\tRegression on %s %d -> %d\n", table[i].name,
                        table[i].success, new_table[j].success);
                errors++;
              }
            }
          }
        }
      }
    }
  }
  fprintf(stderr, "%d errors\n", errors);
  return errors;
}

/**
 *  * Simple example to parse a file called "file.xml",
 *   * walk down the DOM, and print the name of the
 *    * xml elements nodes.
 *     */
int main(int argc, char **argv) {
  xmlDoc *doc = NULL;
  xmlNode *root_element = NULL;

  sctk_leaf_t *table = NULL;
  int table_size = 0;

  sctk_leaf_t *new_table = NULL;
  int new_table_size = 0;

  if (argc != 3)
    return (1);

  /*
*     * this initialize the library and check potential ABI mismatches
*          * between the version it was compiled for and the actual shared
*               * library used.
*                    */
  LIBXML_TEST_VERSION

  /*parse the file and get the DOM */
  doc = xmlReadFile(argv[1], NULL, 0);

  if (doc == NULL) {
    printf("error: could not parse file %s\n", argv[1]);
  }

  mprintf("Read file %s\n", argv[1]);

  /*Get the root element node */
  root_element = xmlDocGetRootElement(doc);

  print_element_names(root_element, &table_size, &table);

  /*free the document */
  xmlFreeDoc(doc);

  /*
*     *Free the global variables that may
*          *have been allocated by the parser.
*               */
  xmlCleanupParser();

  /*parse the file and get the DOM */
  doc = xmlReadFile(argv[2], NULL, 0);

  if (doc == NULL) {
    printf("error: could not parse file %s\n", argv[2]);
  }

  mprintf("Read file %s\n", argv[2]);

  /*Get the root element node */
  root_element = xmlDocGetRootElement(doc);

  print_element_names(root_element, &new_table_size, &new_table);

  /*free the document */
  xmlFreeDoc(doc);

  /*
 * *     *Free the global variables that may
 * *          *have been allocated by the parser.
 * *               */
  xmlCleanupParser();

  return validate_results(table, table_size, new_table, new_table_size);
}
#else
int main(void) {
  fprintf(stderr, "Tree support not compiled in\n");
  exit(1);
}
#endif
