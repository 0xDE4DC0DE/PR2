import java.util.Iterator;

public class DataBoardTest {
    public static void main(String[] args) {

        //DataBoard<Data> databoardfede = new MyDataBoard<>("fede00", "federico");
        //Per usare la prima implementazione basta commentare il secondo assegnamento, altrimenti commentare il primo
        DataBoard<Data> databoardfede = new MyDataBoard2<>("fede00", "federico");
        System.out.println("");
        System.out.println("PROVO A CREARE UNA CATEGORIA SBAGLIANDO PASSWORD");
        try {
            databoardfede.createCategory("foto", "fede");
        } catch (DuplicateEntryException | WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("CREO UNA CATEGORIA");
        try {
            databoardfede.createCategory("foto", "fede00");
        } catch (DuplicateEntryException | WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO A CREARE UNA CATEGORIA GIA ESISTENTE");
        try {
            databoardfede.createCategory("foto", "fede00");
        } catch (DuplicateEntryException | WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("CREO UN'ALTRA CATEGORIA DA RIMUOVERE");
        try {
            databoardfede.createCategory("video", "fede00");
        } catch (WrongPasswordException | DuplicateEntryException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO AD ELIMINARE L'ULTIMA CATEGORIA CREATA SBAGLIANDO PASSWORD");
        try {
            databoardfede.removeCategory("video", "fede");
        } catch (WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("RIMUOVO L'ULTIMA CATEGORIA CREATA");
        try {
            databoardfede.removeCategory("video", "fede00");
        } catch (WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("PROVO AD AGGIUNGERE UN AMICO SBAGLIANDO PASSWORD");
        try {
            databoardfede.addFriend("foto", "fede", "leonardo");
        } catch (WrongPasswordException | CategoryNotFoundException | DuplicateEntryException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO AD AGGIUNGERE UN AMICO IN UNA CATEGORIA CHE NON ESISTE");
        try {
            databoardfede.addFriend("video", "fede00", "leonardo");
        } catch (WrongPasswordException | CategoryNotFoundException | DuplicateEntryException err) {
            err.printStackTrace();
        }

        System.out.println("AGGIUNGO UN AMICO");
        try {
            databoardfede.addFriend("foto", "fede00", "leonardo");
        } catch (WrongPasswordException | CategoryNotFoundException | DuplicateEntryException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO AD AGGIUNGERE UN AMICO IN UNA CATEGORIA CHE CONTIENE GIA QUELL'AMICO");
        try {
            databoardfede.addFriend("foto", "fede00", "leonardo");
        } catch (WrongPasswordException | CategoryNotFoundException | DuplicateEntryException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("AGGIUNGO UN AMICO DA RIMUOVERE");
        try {
            databoardfede.addFriend("foto", "fede00", "pippo");
        } catch (WrongPasswordException | CategoryNotFoundException | DuplicateEntryException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO A RIMUOVERE L'ULTIMO AMICO AGGIUNTO SBAGLIANDO PASSWORD");
        try {
            databoardfede.removeFriend("foto", "fede", "pippo");
        } catch (WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("RIMUOVO L'ULTIMO AMICO AGGIUNTO");
        try {
            databoardfede.removeFriend("foto", "fede00", "pippo");
        } catch (WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("CREO UN DATO");
        Data<Data> fotoalmare = new MyData<>("fotoalmare");
        System.out.println("PROVO AD AGGIUNGERE UN DATO SBAGLIANDO PASSWORD");
        try {
            databoardfede.put("fede", fotoalmare, "foto");
        } catch (WrongPasswordException | CategoryNotFoundException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO AD AGGIUNGERE UN DATO IN UNA CATEGORIA CHE NON ESISTE");
        try {
            databoardfede.put("fede00", fotoalmare, "video");
        } catch (WrongPasswordException | CategoryNotFoundException err) {
            err.printStackTrace();
        }

        System.out.println("AGGIUNGO IL DATO");
        try {
            databoardfede.put("fede00", fotoalmare, "foto");
        } catch (WrongPasswordException | CategoryNotFoundException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("PROVO A PRENDERE IL DATO CHE HO APPENA AGGIUNTO SBAGLIANDO PASSWORD");
        try {
            databoardfede.get("fede", fotoalmare);
        } catch (WrongPasswordException | ItemNotListedException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO A PRENDERE UN DATO CHE NON ESISTE");
        Data<Data> selfie = new MyData<>("selfie");
        try {
            databoardfede.get("fede00", selfie);
        } catch (WrongPasswordException | ItemNotListedException err) {
            err.printStackTrace();
        }

        System.out.println("PRENDO IL DATO CHE HO INSERITO PRIMA");
        try {
            databoardfede.get("fede00", fotoalmare);
        } catch (WrongPasswordException | ItemNotListedException err) {
            err.printStackTrace();
        }


        System.out.println("");
        System.out.println("AGGIUNGO UN DATO DA RIMUOVERE");
        try {
            databoardfede.put("fede00", selfie, "foto");
        } catch (WrongPasswordException | CategoryNotFoundException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO A RIMUOVERE L'ULTIMO DATO INSERITO SBAGLIANDO PASSWORD");
        try {
            databoardfede.remove("fede", selfie);
        } catch (WrongPasswordException | ItemNotListedException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO A RIMUOVERE UN DATO CHE NON ESISTE");
        Data<Data> tramonto = new MyData<>("tramonto");
        try {
            databoardfede.remove("fede00", tramonto);
        } catch (WrongPasswordException | ItemNotListedException err) {
            err.printStackTrace();
        }

        System.out.println("RIMUOVO L'ULTIMO DATO AGGIUNTO");
        try {
            databoardfede.remove("fede00", selfie);
        } catch (WrongPasswordException | ItemNotListedException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("PROVO A METTERE LIKE A UN DATO CHE NON ESISTE");
        try {
            databoardfede.insertLike("leonardo", tramonto);
        } catch (FriendWithoutPermissionException | FriendAlreadyLikedException | ItemNotListedException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO A METTERE LIKE CON UN AMICO CHE NON PUO");
        try {
            databoardfede.insertLike("pippo", fotoalmare);
        } catch (FriendAlreadyLikedException | FriendWithoutPermissionException | ItemNotListedException err) {
            err.printStackTrace();
        }

        System.out.println("METTO LIKE CON UN AMICO CHE PUO");
        try {
            databoardfede.insertLike("leonardo", fotoalmare);
        } catch (FriendWithoutPermissionException | ItemNotListedException | FriendAlreadyLikedException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO A METTERE LIKE CON UN AMICO CHE HA GIA MESSO LIKE");
        try {
            databoardfede.insertLike("leonardo", fotoalmare);
        } catch (FriendAlreadyLikedException | FriendWithoutPermissionException | ItemNotListedException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("PROVO A PRENDERE LA LISTA DI TUTTI I DATI INSERITI IN UNA CATEGORIA SBAGLIANDO PASSWORD");
        try {
            databoardfede.getDataCategory("fede", "foto");
        } catch (WrongPasswordException | CategoryNotFoundException err) {
            err.printStackTrace();
        }

        System.out.println("PROVO A PRENDERE LA LISTA DI TUTTI GLI ELEMENTI IN UNA CATEGORIA CHE NON ESISTE");
        try {
            databoardfede.getDataCategory("fede00", "video");
        } catch (WrongPasswordException | CategoryNotFoundException err) {
            err.printStackTrace();
        }

        System.out.println("PRENDO LA LISTA DI TUTTI I DATI INSERITI IN UNA CATEGORIA");
        try {
            databoardfede.getDataCategory("fede00", "foto");
        } catch (WrongPasswordException | CategoryNotFoundException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("PROVO A PRENDERE LA LISTA ORDINATA PER LIKE DI TUTTI I DATI SBAGLIANDO PASSWORD");
        try {
            databoardfede.getIterator("fede");
        } catch (WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("PRENDO LA LISTA ORDINATA IN BASE AI LIKE DI TUTTI I DATI INSERITI");
        try {
            databoardfede.getIterator("fede00");
        } catch (WrongPasswordException err) {
            err.printStackTrace();
        }

        System.out.println("");
        System.out.println("PROVO A PRENDERE LA LISTA DI TUTTI I DATI CONDIVISI CON UN AMICO CHE NON HO INSERITO");
        try {
            databoardfede.getFriendIterator("pippo");
        } catch (FriendNeverListedException err) {
            err.printStackTrace();
        }

        System.out.println("PRENDO LA LISTA DI TUTTI I DATI CONDIVISI CON UN AMICO");
        Iterator<Data> iterator = null;
        try {
            iterator = databoardfede.getFriendIterator("leonardo");
        } catch (FriendNeverListedException err) {
            err.printStackTrace();
        }

        System.out.println("CONTROLLO CHE LA LISTA DEI DATI CONDIVISI CON L'AMICO PRECEDENTE STAMPI I DATI CORRETTAMENTE");
        assert iterator != null;
        while (iterator.hasNext()) {
            Data dato = iterator.next();
            dato.Display();
        }

        System.out.println("");
        System.out.println("FACCIO LA DISPLAY DEL DATO CHE HO INSERITO ALL'INIZIO");
        fotoalmare.Display();

    }
}