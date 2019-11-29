import java.util.EmptyStackException;
import java.util.Iterator;
import java.util.List;

public interface DataBoard <E extends Data> {
    /*
    OVERVIEW: insieme modificabile di un insieme con nome di categorie
    TYPICAL ELEMENT: nome, passw, <categories_i> con i>=0

    REP INVARIANT: 0<=i<categories.size
                   name!=null && passw!=null => 0<=i<categories.size()
    AF: AF(databoard) { categories_i con 0<=i<categories.size | categories e' un insieme di categorie
                        di nome name e password passw }
     */

    /**
     * Crea una categoria di dati se vengono rispettati i controlli di identita
     * @param category!=null
     * @param passw!=null
     * @throws WrongPasswordException if passw!=this.passw
     * @throws NullPointerException if category==null || passw==null
     * @throws DuplicateEntryException if categories.contains(category)
     * @modifies categories
     * @effects categories_post = categories_pre U category
     */
    public void createCategory(String category, String passw) throws WrongPasswordException, DuplicateEntryException;


    /**
     * Rimuove una categoria di dati se vengono rispettati i controlli di identita
     * @param category!=null
     * @param passw!=null
     * @throws WrongPasswordException if passw!=this.passw
     * @throws NullPointerException if category==null || passw==null
     * @modifies categories
     * @effects categories_post = categories_pre - category
     */
    public void removeCategory(String category, String passw) throws WrongPasswordException;


    /**
     * Aggiunge un amico ad una categoria di dati se vengono rispettati i controlli di identita
     * @param category!=null
     * @param passw!=null
     * @param friend!=null
     * @throws WrongPasswordException if passw!=this.passw
     * @throws NullPointerException if category==null || passw==null || friend==null
     * @throws DuplicateEntryException if categories_i.contains(friend)==true with categories_i==category
     * @throws CategoryNotFoundException if categories.contains(category)==false
     * @modifies categories.friends
     * @effects categories.friends_post = categories.friends_pre U categories.friend
     */
    public void addFriend(String category, String passw, String friend) throws WrongPasswordException, DuplicateEntryException, CategoryNotFoundException;


    /**
     * Rimuove un amico da una categoria di dati se vengono rispettati i controlli di identita
     * @param category!=null
     * @param passw!=null
     * @param friend!=null
     * @throws WrongPasswordException if passw!=this.passw
     * @throws NullPointerException if category==null || passw==null || friend==null
     * @modifies categories.friends
     * @effects categories.friends_post = categories.friends_pre - categories.friend
     */
    public void removeFriend(String category, String passw, String friend) throws WrongPasswordException;


    /**
     * Inserisce un dato in bacheca se vengono rispettati i controlli di identita
     * @param passw!=null
     * @param dato!=null
     * @param category!=null
     * @return true if
     * @throws WrongPasswordException if passw!=this.passw
     * @throws NullPointerException if category==null || passw==null || dato==null
     * @throws CategoryNotFoundException if categories.contains(category)==false
     * @modifies categories.dati
     * @effects categories.dati_post = categories.dati_pre U dato
     */
    public boolean put(String passw, E dato, String category) throws WrongPasswordException, CategoryNotFoundException;


    /**
     * Restituisce una copia del dato in bacheca se vengono rispettati i controlli di identita
     * @param passw!=null
     * @param dato!=null
     * @return categories_i.dato_k with categories_i.dato_k=dato
     * @throws WrongPasswordException if passw!=this.passw
     * @throws NullPointerException if dato==null || passw==null
     * @throws ItemNotListedException if categories.contains(dato)==false
     * @modifies none
     */
    public E get(String passw, E dato) throws WrongPasswordException, ItemNotListedException;


    /**
     * Rimuove il dato dalla bacheca se vengono rispettati i controlli di identita
     * @param passw!=null
     * @param dato!=null
     * @return categories_i.dato_k with categories_i.dato_k=dato
     * @throws WrongPasswordException if passw!=this.passw
     * @throws NullPointerException if dato==null || passw==null
     * @throws ItemNotListedException if categories.contains(dato)==false
     * @modifies categories.dati
     * @effects categories.dati_post = categories.dati_pre - categories.dato
     */
    public E remove(String passw, E dato) throws WrongPasswordException, ItemNotListedException;


    /**
     * Aggiunge un like a un dato se vengono rispettati i controlli di identita
     * @param friend!=null
     * @param dato!=null
     * @throws FriendAlreadyLikedException if categories.dato.contains(friend)==true
     * @throws NullPointerException if friend==null || dato==null
     * @throws ItemNotListedException if categories.contains(dato)==false
     * @throws FriendWithoutPermissionException if categories.!contains(friend)
     * @modifies categories.dati
     * @effects categories.dato.like = categories.dato.like+1
     */
    void insertLike(String friend, E dato) throws FriendAlreadyLikedException, ItemNotListedException, FriendWithoutPermissionException;


    /**
     * Crea la lista dei dati in bacheca di una determinata categoria se vengono rispettati i controlli di identita
     * @param passw!=null
     * @param category!= null
     * @return list = categories_i.dato U categories_i+1.dato forall i<=0<categories.size-1
     * @throws NullPointerException if passw==null || category==null
     * @throws WrongPasswordException if passw!=this.passw
     * @throws CategoryNotFoundException if categories.!contains(category)
     * @modifies none
     */
    public List<E> getDataCategory(String passw, String category) throws WrongPasswordException, CategoryNotFoundException;


    /**
     * restituisce un iteratore (senza remove) che genera tutti i dati in bacheca ordinati in ordine decrescente
     * rispetto al numero di like, se vengono rispettati i controlli di identita
     * @param passw!=null
     * @return orderedlist.iterator, ovvero una lista di dati ordinata in base ai like convertita in iterator
     * @throws WrongPasswordException if passw!=this.passw
     * @throws NullPointerException if passw==null
     * @modifies none
     */
    public Iterator<E> getIterator(String passw) throws WrongPasswordException;


    /**
     * Restituisce un iteratore (senza remove) che genera tutti i dati in bacheca condivisi
     * @param friend!=null
     * @return list.iterator, ovvero una lista dei dati che sono condivisi con l'amico friend convertita in iterator
     * @throws NullPointerException if friend==null
     * @throws FriendNeverListedException if categories_i.!contains(friend) forall 0<=i<categories.size
     * @modifies none
     */
    public Iterator<E> getFriendIterator(String friend) throws FriendNeverListedException;


    //Altre operazione da definire a scelta


    /**
     * Restituisce nella prima implementazione l'indice della categoria passata come riferimento
     * Nella seconda implementazione @throws UnsoppurtedOperationException ogni volta che viene invocato
     * @param category!=null
     * @return i with category_i=category
     * @throws NullPointerException if category==null
     * @throws EmptyStackException if caregories_i.size()==0
     * @modifies none
     */
    public int indexOf(String category);
}