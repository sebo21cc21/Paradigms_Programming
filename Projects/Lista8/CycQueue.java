import java.util.ArrayList;
import java.util.List;

public class CycQueue<E> implements MyQueue<E>{

    private final int size;
    private int front, rear;
    private final List<E> arrList;

    public CycQueue(int size) {
        this.arrList = new ArrayList<>(size);
        this.front = this.rear = -1;
        this.size = size;
    }

    @Override
    public void enqueue(E x) throws FullException {
        if (isFull())
            throw new FullException("Queue is full (enqueue met CycQueue)");
        else if (isEmpty())
            front++;
        rear = (rear + 1) % size;
        arrList.add(rear, x);
    }

    @Override
    public void dequeue() {
        if (!isEmpty()) {
            if (front == rear)
                front = rear = -1;
            else
                front = (front + 1) % size;
        }
    }

    @Override
    public E first() throws EmptyException {
        if (isEmpty())
            throw new EmptyException("Queue is empty (first met CycQueue)");
        return arrList.get(front);
    }

    @Override
    public boolean isEmpty() {
        return front == -1;
    }

    @Override
    public boolean isFull() {
        return (rear + 1) % size == front;
    }
}
