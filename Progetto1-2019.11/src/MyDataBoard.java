import java.util.*;

public class MyDataBoard<E extends Data> implements DataBoard<E> {

    private String passw;
    private String name;
    final private Vector<Category<E>> categories;

    public MyDataBoard(String nome, String passw) {
        if (nome==null || passw==null) throw new NullPointerException();
        categories = new Vector<>();
        this.name=nome;
        this.passw=passw;
    }

    public int indexOf(String category) {
        if (category==null) throw new NullPointerException();
        int i=0;
        while (i<categories.size()) {
            if (categories.get(i).getName().equals(category)) return i;
            i++;
        }
        return -1;
    }

    public void createCategory (String category, String passw) throws WrongPasswordException, DuplicateEntryException {
        if (category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        if (indexOf(category)>=0) throw new DuplicateEntryException();
        categories.addElement(new MyCategory<E>(category));
        }

    public void removeCategory (String category, String passw) throws WrongPasswordException {
        if (category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        int i = indexOf(category);
        if (i>=0) categories.remove(i);
    }

    public void addFriend (String category, String passw, String friend) throws WrongPasswordException, DuplicateEntryException, CategoryNotFoundException {
        if (friend==null || category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        int i = indexOf(category);
        if (i<0) throw new CategoryNotFoundException();
        categories.get(i).addFriend(friend);
    }

    public void removeFriend (String category, String passw, String friend) throws WrongPasswordException {
        if (friend==null || category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        int i = indexOf(category);
        if (i>=0) categories.get(i).removeFriend(friend);
    }

    public boolean put (String passw, E dato, String category) throws WrongPasswordException, CategoryNotFoundException {
        if (dato==null || category==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        int i = indexOf(category);
        if (i<0) throw new CategoryNotFoundException();
        return categories.get(i).addDato(dato);
    }

    public E get (String passw, E dato) throws WrongPasswordException, ItemNotListedException {
        if (dato==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        int i=0;
        while (i<categories.size()) {
            if (categories.get(i).contains(dato)) return categories.get(i).getDato(dato);
            else i++;
        }
        throw new ItemNotListedException();
    }

    public E remove (String passw, E dato) throws WrongPasswordException, ItemNotListedException {
        if (dato==null || passw==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        int i=0;
        while (i<categories.size()) {
            if (categories.get(i).contains(dato)) {
                E dato1 = categories.get(i).getDato(dato);
                categories.get(i).removeDato(dato);
                return dato1;
            } else i++;
        }
        throw new ItemNotListedException();
    }

    public void insertLike (String friend, E dato) throws FriendAlreadyLikedException, ItemNotListedException, FriendWithoutPermissionException {
        if (dato==null || friend==null) throw new NullPointerException();
        int i=0;
        int esiste=0;
        while (i<categories.size()) {
            if (categories.get(i).contains(dato)) {
                if (!categories.get(i).containsFriend(friend)) throw new FriendWithoutPermissionException();
                categories.get(i).getDato(dato).addLike(friend);
                esiste=1;
            }
            i++;
        }
        if (esiste==0) throw new ItemNotListedException();
    }

    public List<E> getDataCategory (String passw, String category) throws WrongPasswordException, CategoryNotFoundException {
        if (passw==null || category==null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        if (indexOf(category)<0) throw new CategoryNotFoundException();
        if (categories.get(indexOf(category)).listData().size()==0) throw new EmptyStackException();
        return categories.get(indexOf(category)).listData();
    }

    public Iterator<E> getIterator (String passw) throws WrongPasswordException {
        if (passw == null) throw new NullPointerException();
        if (!this.passw.equals(passw)) throw new WrongPasswordException();
        int i = 0;
        List<E> list = new ArrayList<>();
        while (i < categories.size()) {
            list.addAll(categories.get(i).listData());
            i++;
        }
        if (list.size()==0) throw new EmptyStackException();
        list.sort(new DataComparator());
        return Collections.unmodifiableList(list).iterator();
    }

    public Iterator<E> getFriendIterator (String friend) throws FriendNeverListedException {
        if (friend ==null) throw new NullPointerException();
        int i = 0;
        List<E> list = new ArrayList<>();
        while (i < categories.size()) {
            if (categories.get(i).containsFriend(friend)) {
                list.addAll(categories.get(i).listData());
            }
            i++;
        }
        if (list.size()==0) throw new FriendNeverListedException();
        return Collections.unmodifiableList(list).iterator();
    }
}