package steshaw;

import java.util.List;
import java.util.ArrayList;

class PagingJava {
  private static <T> List<T> getPage(List<T> list, int pageNumber, int pageSize) {
    List<T> results = new ArrayList<T>();
    if (list.size() <= pageSize) {
      return list;
    }
    int offset = (pageNumber - 1) * pageSize;
    if (offset < 0) {
      offset = 0;
    }
    for (; offset < pageSize && offset < list.size(); offset++) {
      results.add(list.get(offset));
    }
    return results;
  }

  private static <T> List<T> getPageWithBugFixed(List<T> list, int pageNumber, int pageSize) {
    List<T> results = new ArrayList<T>();
    if (list.size() <= pageSize) {
      return list;
    }
    int offset = (pageNumber - 1) * pageSize;
    if (offset < 0) {
      offset = 0;
    }
    int last = offset + pageSize;
    for (; offset < last && offset < list.size(); offset++) {
      results.add(list.get(offset));
    }
    return results;
  }

  public static void fill(List<Integer> l, int count) {
    for (int i = 1; i <= count; ++i) l.add(i);
  }

  public static void main(String[] args) {
    List<Integer> l = new ArrayList<Integer>();
    fill(l, 1000);
    int pageSize = 6;

    for (int page = -10; page <= 200; ++page) {
      System.out.println("" + page + " - " + getPage(l, page, pageSize));
    }

    List<Integer> big = new ArrayList<Integer>();
    fill(big, 100000);
    for (int i = 1; i <= 10; ++i) {
      System.gc();
      long start = System.currentTimeMillis();
      for (int page = -10; page <= 20000; ++page) getPage(big, page, pageSize).size();
      System.err.println("took " + (System.currentTimeMillis() - start) + "ms");
    }
  }
}
