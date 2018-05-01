#include<graphics.h>
#include<conio.h>
#include<stdio.h>
#include<dos.h>

int initmouse();
void showmouseptr();
void hidemouseptr();
void getmousepos(int*,int*,int*);
void setposi(int, int);
union REGS i, o;

main()
{
   int gd = DETECT, gm, status, button, x, y, tempx, tempy;
   char array[50];

   initgraph(&gd,&gm,"C:\\TURBOC3\\BGI");
   settextstyle(DEFAULT_FONT,0,2);
   setcolor(WHITE);
   circle(311,223,200);
   status = initmouse();

   if ( status == 0 )
      printf("Mouse support not available.\n");
   else
   {
      showmouseptr();

      getmousepos(&button,&x,&y);

      tempx = x;
      tempy = y;

      while(!kbhit())
      {
	 getmousepos(&button,&x,&y);

	  setcolor(WHITE);
	 circle(311,223,200);


	 if( x == tempx && y == tempy )
	 {}

	 else
	 {
	    cleardevice();
	    sprintf(array,"X = %d, Y = %d",x,y);
	    outtext(array);
	    tempx = x;
	    tempy = y;
	 }
	 while( x!=311 && y!=223 ){
		if(x < 311 ){
			x++;
			}
		else if( x > 311 ){
			x--;
			}
		if( y < 223){
			y++;
			}
		else if( y> 223){
			y--;
			}
		delay(20);
		setposi(x,y);
	 }
      }
   }

   getch();
   return 0;
}

int initmouse()
{
   i.x.ax = 0;
   int86(0X33, &i, &o);
   return ( o.x.ax );
}

void showmouseptr()
{
   i.x.ax = 1;
   int86(0X33, &i, &o);
}

void getmousepos(int *button, int *x, int *y)
{
   i.x.ax = 3;
   int86(0X33, &i, &o);

   *button = o.x.bx;
   *x = o.x.cx;
   *y = o.x.dx;
}
void setposi(int xp,int yp){
	i.x.ax = 4;
	i.x.cx = xp;
	i.x.dx = yp;
	int86(0X33, &i, &o);
}