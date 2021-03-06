public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        String result = content.substring(this.interval.getX(), this.interval.getY());
        for(int i=0; i < result.length(); i++) {
            this.buffer[this.offset + i] = result.charAt(i);
        }
    }
}