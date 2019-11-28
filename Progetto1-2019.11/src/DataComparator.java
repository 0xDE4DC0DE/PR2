import java.util.Comparator;

public class DataComparator implements Comparator<Data> {

    @Override
    public int compare(Data o1, Data o2) {
        if (o1.getLike()>o2.getLike()) return -1;
        else if (o1.getLike()<o2.getLike()) return 1;
        return 0;
    }
}
