/**
 * Muestra por pantalla la frase "Hola Mundo"
 * 
 * @author Agustin Lopez
 */
public class HolaMundo {
    public static void main (String[] args) {
        String naranja = "\033[33m";
        String azul = "\033[34m";
        System.out.println(naranja + "Hola mundo");
        System.out.println(azul + "Aqui estamos haciendo pruebas con Git.");
    }
}