package de.weitz;
import javax.swing.*;
import java.awt.*;

class Plane extends JPanel {
  int size;
  boolean points[][];
  Plane(PointSetter ps, int size) {
    this.size = size;
    this.points = new boolean[size][size];
    ps.fill(points);
    this.setPreferredSize(new Dimension(size, size));
  }
  public void paintComponent(Graphics g) {
    super.paintComponent(g);
    for (int x = 0; x < size; x++) {
      for (int y = 0; y < size; y++) {
        ((Graphics2D) g).setPaint(points[x][y] ?
                                  Color.black : Color.white);
        ((Graphics2D) g).drawLine(x, y, x, y);
      }
    }
  }
}

public class Mandelbrot extends JFrame {
  public Mandelbrot(PointSetter ps, int size) {
    final Plane plane = new Plane(ps, size);
    add(plane);
    setTitle("Mandelbrot");
    pack();
  }
  public void display() {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
        public void run() {
          setVisible(true);
        }
      });
  }
}
