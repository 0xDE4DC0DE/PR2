import java.util.Vector;

public interface Data<E> {
    /*
    OVERVIEW: insieme modificabile di un dato con il suo numero di like e l'insieme degli amici che hanno messo like
    TYPICAL ELEMENT: <dato, like, friends_0>, ..., <dato, like, friends_last>

    REP INVARIANT: (dato!=null) && (like>=0) && (like=0)<=>(friends=null) && (like=friends.size)
    AF: AF (Data) = { friends_i con 0<=i<size | l'oggetto in friends_i e' uno degli amici che hanno messo like, size e'
                      il numero di like associati al dato }
    */

    /**
     * Stampa a video il dato inserito, il numero di like e la lista di amici che hanno messo like se like!=0
     * @requires none
     */
    public void Display();

    /**
     * Ritorna il numero di like
     * @return like
     */
    public int getLike();

    /**
     * Aggiunge un like e il nome dell'amico che lo ha messo
     * @param friend!=null
     * @throws IllegalArgumentException if friend==null
     * @throws FriendAlreadyLikedException if friends.contains(friend)
     * @modifies like && friends
     * @effects like_post = like_pre + 1
     * @effects friends_post = friend_pre U friend
     */
    public void addLike(String friend) throws FriendAlreadyLikedException;

    /**
     * Ritorna il nome del dato
     * @requires none
     * @return name
     */
    public String getName ();

    /**
     * Setta i like del dato a quelli specificati [assati come paramentro
     * @param like
     */
    public void setLike(int like);

    /**
     * Setta il vettore di amici a quello passato come parametro
     * @param friends
     */
    public void setFriends (Vector<String> friends);

    /**
     * Ritorna il vettore di amici
     * @return this.friends
     */
    public Vector<String> getFriends();
}
