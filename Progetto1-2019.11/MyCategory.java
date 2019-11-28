import java.util.Collections;
import java.util.List;
import java.util.Vector;

public class MyCategory<E extends Data> implements Category<E>, Cloneable {

    final private Vector<String> friends;
    final private Vector<E> dati;
    private String name;

    public MyCategory (String name) {
        if (name==null) throw new NullPointerException();
        friends = new Vector<>();
        dati = new Vector<>();
        this.name = name;
    }

    public void addFriend (String friend) throws DuplicateEntryException {
        if (friend==null) throw new NullPointerException();
        if (friends.contains(friend)) throw new DuplicateEntryException();
        friends.addElement(friend);
    }

    public void removeFriend (String friend) {
        if (friend==null) throw new NullPointerException();
        friends.remove(friend);
    }

    public boolean addDato (E dato) {
        if (dato==null) throw new NullPointerException();
        if (dati.contains(dato)) return false;
        dati.addElement(dato);
        return true;
    }

    public E getDato (E dato) {
        if (dato==null) throw new NullPointerException();
        int i = dati.indexOf(dato);
        E clone = (E) new MyData(dati.get(i).getName());
        clone.setLike(dati.get(i).getLike());
        clone.setFriends(dati.get(i).getFriends());
        return clone;
    }

    public E returnDato (E dato) {
        if (dato==null) throw new NullPointerException();
        int i = dati.indexOf(dato);
        return dati.get(i);
    }

    public void removeDato (E dato) {
        if (dato==null) throw new NullPointerException();
        dati.remove(dato);
    }

    public boolean contains (E dato) {
        if (dato == null) throw new NullPointerException();
        int i = 0;
        while (i < dati.size()) {
            if (dati.get(i).getName().equals(dato.getName())) return true;
            i++;
        }
        return false;
    }

    public String getName() {

        return name;
    }

    public List <E> listData() {
        return Collections.unmodifiableList(dati);
    }

    public boolean containsFriend(String friend) {
        if (friend==null) throw new NullPointerException();
        return friends.contains(friend);
    }

}