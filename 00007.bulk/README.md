# [BULK - The Bulk!](https://www.spoj.com/problems/BULK/)

ACM uses a new special technology of building its transceiver stations.  This technology is called *Modular Cuboid Architecture* (*MCA*) and is covered by a patent of Lego company.  All parts of the transceiver are shipped in unit blocks that have the form of cubes of exactly the same size.  The cubes can be then connected to each other.  The MCA is modular architecture, that means we can select preferred transceiver configuration and buy only those components we need.

The cubes must be always connected "face-to-face", i.e. the whole side of one cube is connected to the whole side of another cube.  One cube can be thus connected to at most six other units.  The resulting equipment, consisting of unit cubes is called *The Bulk* in the communication technology slang.

Sometimes, an old and unneeded bulk is condemned, put into a storage place, and replaced with a new one.  It was recently found that ACM has many of such old bulks that just occupy space and are no longer needed.  The director has decided that all such bulks must be disassembled to single pieces to save some space.  Unfortunately, there is no documentation for the old bulks and nobody knows the exact number of pieces that form them.  You are to write a computer program that takes the bulk description and computes the number of unit cubes.

Each bulk is described by its faces (sides).  A special X-ray based machine was constructed that is able to localise all faces of the bulk in the space, even the inner faces, because the bulk can be partially hollow (it can contain empty spaces inside).  But any bulk must be connected (i.e. it cannot drop into two pieces) and composed of whole unit cubes.

## Input

There is a single positive integer $T$ on the first line of input (equal to about 1000).  It stands for the number of bulks to follow.  Each bulk description begins with a line containing single positive integer $F, 6 \le F \le 250$, stating the number of faces.  Then there are $F$ lines, each containing one face description.  All faces of the bulk are always listed, in any order.  Any face may be divided into several distinct parts and described like if it was more faces.  Faces do not overlap.  Every face has one inner side and one outer side.  No side can be "partially inner and partially outer".

Each face is described on a single line.  The line begins with an integer number $P$ stating the number of points that determine the face, $4 \le P \le 200$.  Then there are $3 \times P$ numbers, coordinates of the points.  Each point is described by three coordinates $X, Y, Z (0 \le X,Y,Z\,\le 1000)$ separated by spaces.  The points are separated from each other and from the number $P$ by two space characters.  These additional spaces were added to make the input more human readable.  The face can be constructed by connecting the points in the specified order, plus connecting the last point with the first one.

The face is always composed of "unit squares", that means every edge runs either in $X, Y$ or $Z$-axis direction.  If we take any two neighbouring points $X_1, Y_1, Z_1$ and $X_2, Y_2, Z_2$, then the points will always differ in exactly one of the three coordinates.  I.e. it is either $X_1 \ne X_2$, or $Y_1 \ne Y_2$, or $Z_1 \ne Z_2$, other two coordinates are the same.  Every face lies in an orthogonal plane, i.e. exactly one coordinate is always the same for all points of the face.  The face outline will never touch nor cross itself.

## Output

Your program must print a single line for every test case.  The line must contain the sentence `The bulk is composed of V units.`, where $V$ is the volume of the bulk.

## Example

Sample Input:

```
2
12
4  10 10 10  10 10 20  10 20 20  10 20 10
4  20 10 10  20 10 20  20 20 20  20 20 10
4  10 10 10  10 10 20  20 10 20  20 10 10
4  10 20 10  10 20 20  20 20 20  20 20 10
4  10 10 10  10 20 10  20 20 10  20 10 10
5  10 10 20  10 20 20  20 20 20  20 15 20  20 10 20
4  14 14 14  14 14 16  14 16 16  14 16 14
4  16 14 14  16 14 16  16 16 16  16 16 14
4  14 14 14  14 14 16  16 14 16  16 14 14
4  14 16 14  14 16 16  16 16 16  16 16 14
4  14 14 14  14 16 14  16 16 14  16 14 14
4  14 14 16  14 16 16  16 16 16  16 14 16
12
4  20 20 30  20 30 30  30 30 30  30 20 30
4  10 10 10  10 40 10  40 40 10  40 10 10
6  10 10 20  20 10 20  20 30 20  30 30 20  30 40 20  10 40 20
6  20 10 20  20 20 20  30 20 20  30 40 20  40 40 20  40 10 20
4  10 10 10  40 10 10  40 10 20  10 10 20
4  10 40 10  40 40 10  40 40 20  10 40 20
4  20 20 20  30 20 20  30 20 30  20 20 30
4  20 30 20  30 30 20  30 30 30  20 30 30
4  10 10 10  10 40 10  10 40 20  10 10 20
4  40 10 10  40 40 10  40 40 20  40 10 20
4  20 20 20  20 30 20  20 30 30  20 20 30
4  30 20 20  30 30 20  30 30 30  30 20 30
```

Sample Output:

```
The bulk is composed of 992 units.
The bulk is composed of 10000 units.
```

**Warning: large Input/Output data, be careful with certain languages.**

## Solution

[main.ml](main.ml)
