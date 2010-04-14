import java.util.Date;
import java.util.List;
import java.util.Arrays;
import java.math.BigInteger;

class ListToString {

    public static class Track {
      public Track(BigInteger id, String name, BigInteger downloadCount, Date added, Date published) {
        this.id = id;
        this.name = name;
        this.downloadCount = downloadCount;
        this.added = added;
        this.published = published;
      }
      BigInteger id;
      String name;
      BigInteger downloadCount;
      Date added;
      Date published;
    }

    private static int maxLengthToReturn = 65536;

    private static String tracksToString(List<Track> tracks) {
        StringBuilder sb = new StringBuilder();
        for (Track track : tracks) {
            if (sb.toString().length() != 0) {
              sb.append(", \n");
            }
            sb.append("{")
              .append("[").append(track.id).append("]").append(",")
              .append("'").append(track.name).append("'").append(",")
              .append("[").append(track.downloadCount).append("]").append(",")
              .append("[").append(track.added).append("]").append(",")
              .append("[").append(track.published).append("]")
              .append("}");
        }
        String result = sb.toString();

        // Truncate if necessary.
        if (result.length() > maxLengthToReturn) {
            result = result.substring(0, maxLengthToReturn);
        }
        return result;
    }

    public static void main(String[] args) {
      Track t1 = new Track(BigInteger.valueOf(1), "Foo", BigInteger.valueOf(0), new Date(110, 3, 1), new Date(109, 0, 1));
      Track t2 = new Track(BigInteger.valueOf(2), "Fred", BigInteger.valueOf(101), new Date(110, 0, 1), new Date(86, 11, 31));
      Track t3 = new Track(BigInteger.valueOf(3), "Wilma", BigInteger.valueOf(327), new Date(110, 1, 28), new Date(103, 5, 30));
      Track t4 = new Track(BigInteger.valueOf(4), "Betty", BigInteger.valueOf(1001), new Date(110, 3, 15), new Date(108, 1, 29));
      System.out.println(tracksToString(Arrays.asList(new Track[] {t1, t2, t3, t4})));
    }

}
