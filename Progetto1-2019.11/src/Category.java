import java.util.List;

public interface Category<E extends Data> {
    /*
    OVERVIEW: insieme modificabile di insieme di data e insieme di friends che possono mettere like
    TYPICAL ELEMENT: <name, data_i, friends_k> con i>=0 & k>=0

    REP INVARIANT: 0<=i<dati.size && 0<=k<friends.size
    AF: AF(category) = { data_i con 0<=i<data.size | l'oggetto contenuto in data_i rappresenta un dato associato alla
                         categoria name e condiviso con gli amici friends }
     */

    /**
     * @param friend!=null
     * @throws DuplicateEntryException if friends.contains(friend)
     * @throws NullPointerException if friend==null
     */
    public void addFriend(String friend) throws DuplicateEntryException;


    /**
     * @param friend!=null
     * @throws NullPointerException if friend==null
     */
    public void removeFriend (String friend);


    /**
     * @param dato!=null
     * @throws NullPointerException if dato==null
     * @return true if dati.add(dato)==true else false
     */
    public boolean addDato(E dato);


    /**
     * Ritorna un clone del dato passato per riferimento
     * @param dato!=null
     * @throws NullPointerException if dato==null
     * @return data_i with data_i==dato
     */
    public E getDato (E dato);


    /**
     * Rimuove il dato passato per riferimento dai dati. Se non si trova dentro i dati non fa niente
     * @param dato!=null
     * @throws NullPointerException if dato==null
     */
    public void removeDato (E dato);


    /**
     * Cerca un dato all'interno di dati. Se lo trova ritorna true, altrimenti false
     * @param dato!=null
     * @throws NullPointerException if dato==null
     * @return true if dati.contains(dato)
     */
    public boolean contains(E dato);


    /**
     * Ritorna il nome della categoria
     * @return category.name
     */
    public String getName ();


    /**
     * Ritorna i dati sotto forma di una lista non modificabile
     * @return list(dati)
     * @modifies none
     */
    public List<E> listData();


    /**
     * Ritorna gli amici sotto forma di lista non modificabile
     * @requires friend != null
     * @throws NullPointerException if friend==null
     * @return true if friends.contains(friend) else false
     * @modifies none
     */
    public boolean containsFriend(String friend);


    /**
     *  Ritorna il dato passato per riferimento
     * @param dato!=null
     * @throws NullPointerException if dato==null
     * @return data_i with data_i==dato
     */
    public E returnDato (E dato);

}