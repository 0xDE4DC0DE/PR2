import java.util.Vector;

public class MyData <E> implements Data<E> {

    private int like;
    private String name;
    final private Vector<String> friends;

    public MyData (String name) {
        if (name==null) throw new NullPointerException();
        friends = new Vector<>();
        this.name = name;
        like=0;
    }

    public void Display () {
        System.out.println("nome: "+name);
        System.out.println("#like: "+like);
        if (like>0) System.out.println("amici che hanno messo like: "+friends);
    }

    public String getName () {
        return name;
    }

    public int getLike () {
        return like;
    }

    public void addLike (String friend) throws FriendAlreadyLikedException {
        if (friend == null) throw new NullPointerException();
        if (friends.contains(friend)) throw new FriendAlreadyLikedException();
        friends.addElement(friend);
        like++;
    }

    public void setLike (int like) {
        this.like=like;
    }

    public void setFriends (Vector<String> friends) {
        this.friends=friends;
    }

    public Vector<String> getFriends () {
        return friends;
    }

}
