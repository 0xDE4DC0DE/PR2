import java.util.*;

public class MyDataBoard2<E extends Data> implements DataBoard<E> {

    private String passw;
    private String name;
    final private HashMap<String, Category<E>> categories;

    public MyDataBoard2(String name, String passw) {
        if (name==null || passw==null) throw new NullPointerException();
        categories = new HashMap<>();
        this.passw = passw;
        this.name = name;
    }

    //In questa implementazione non viene utilizzata la indexOf, la tengo solo perché serve nell'altra implementazione
    //  per non scrivere una nuova interfaccia
    public int indexOf(String category) {
        throw new UnsupportedOperationException("Non é possibile usare questo metodo in DataBoard 2");
    }

    public void createCategory (String category, String passw) throws WrongPasswordException, DuplicateEntryException {
        if (category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        if (categories.containsKey(category)) throw new DuplicateEntryException();
        categories.put(category, new MyCategory<E>(category));
    }

    public void removeCategory (String category, String passw) throws WrongPasswordException {
        if (category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        categories.remove(category);
    }

    public void addFriend (String category, String passw, String friend) throws WrongPasswordException, DuplicateEntryException, CategoryNotFoundException {
        if (friend==null || category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        if (!categories.containsKey(category)) throw new CategoryNotFoundException();
        categories.get(category).addFriend(friend);
    }

    public void removeFriend (String category, String passw, String friend) throws WrongPasswordException {
        if (friend==null || category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        if (categories.containsKey(category)) categories.get(category).removeFriend(friend);
    }

    public boolean put (String passw, E dato, String category) throws WrongPasswordException, CategoryNotFoundException {
        if (dato==null || category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        if (!categories.containsKey(category)) throw new CategoryNotFoundException();
        return categories.get(category).addDato(dato);
    }

    public E get (String passw, E dato) throws WrongPasswordException, ItemNotListedException {
        if (dato==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        E x = null;
        for (Category<E> cat: categories.values()) {
            if (cat.contains(dato)) x=cat.getDato(dato);
        }
        if (x==null) throw new ItemNotListedException();
        return x;
    }

    public E remove (String passw, E dato) throws WrongPasswordException, ItemNotListedException {
        if (dato==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        for (Category<E> cat: categories.values()) {
            if (cat.contains(dato)) {
                E dato1 = cat.getDato(dato);
                cat.removeDato(dato);
                return dato1;
            }
        }
        throw new ItemNotListedException();
    }

    public void insertLike (String friend, E dato) throws FriendAlreadyLikedException, ItemNotListedException, FriendWithoutPermissionException {
        if (dato==null || friend==null) throw new NullPointerException();
        int esiste=0;
        for (Category<E> cat: categories.values()) {
            if (cat.listData().contains(dato)) {
                if (!cat.containsFriend(friend)) throw new FriendWithoutPermissionException();
                cat.returnDato(dato).addLike(friend);
                esiste=1;
            }
        }
        if (esiste==0) throw new ItemNotListedException();
    }

    public List<E> getDataCategory (String passw, String category) throws WrongPasswordException, CategoryNotFoundException {
        if (passw==null || category==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        if (!categories.containsKey(category)) throw new CategoryNotFoundException();
        List<E> lista = categories.get(category).listData();
        if (lista.size()==0) throw new EmptyStackException();
        return lista;
    }

    public Iterator<E> getIterator (String passw) throws WrongPasswordException {
        if (passw == null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        List<E> list = new ArrayList<>();
        for (Category<E> cat : categories.values()) {
            list.addAll(cat.listData());
        }
        if (list.size()==0) throw new EmptyStackException();
        list.sort(new DataComparator());
        return Collections.unmodifiableList(list).iterator();
    }

    public Iterator<E> getFriendIterator (String friend) throws FriendNeverListedException {
        if (friend ==null) throw new NullPointerException();
        List<E> list = new ArrayList<>();
        for (Category<E> cat: categories.values()) {
            if (cat.containsFriend(friend)) {
                list.addAll(cat.listData());
            }
        }
        if (list.size()==0) throw new FriendNeverListedException();
        return Collections.unmodifiableList(list).iterator();
    }
}
