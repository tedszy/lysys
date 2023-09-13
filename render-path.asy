import graph;
import palette;
size(500);

real[][] data = input("path-vertices.dat").line().dimension(0,0);
data = transpose(data);

real[] x = data[0];
real[] y = data[1];

x.cyclic = true;
y.cyclic = true;

//pen[] Palette = Rainbow();
//pen[] Palette = BWRainbow();
pen[] Palette = BWRainbow2();
//pen[] Palette = Grayscale();
//pen[] Palette = Wheel();
Palette.cyclic = true;

path rpath = graph(x, y);

for (int i=0; i < x.length-1; ++i) {
  draw((x[i],y[i])--(x[i+1],y[i+1]), Palette[i]+linewidth(0.1pt));
}

// draw(rpath, blue+linewidth(0.1pt));
// draw(graph(x,y), blue+linewidth(0.1pt));
