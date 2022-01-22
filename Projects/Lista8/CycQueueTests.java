public class CycQueueTests {
    public static void main(String[] args) throws FullException, EmptyException {
        System.out.println("TESTS for CycQueue");
        System.out.println("=================");

        MyQueue<String> queue = new CycQueue<>(3);
        queue.enqueue("Jaś");
        System.out.println(queue.first());
        queue.enqueue("Nina");
        queue.enqueue("Janek");
        //queue.enqueue("Franek");
        System.out.println("full? " + queue.isFull());
        queue.dequeue();
        queue.enqueue("Kinga");
        System.out.println(queue.first());
        queue.dequeue();
        System.out.println(queue.first());
        queue.dequeue();
        System.out.println(queue.first());
        queue.dequeue();
        System.out.println("empty? " + queue.isEmpty());
        //System.out.println(queue.first());
    }
}
