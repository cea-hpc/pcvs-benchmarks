#include "defines.h"
#include <iostream>
#include <curses.h>
using namespace std;

int main(int argc, char *argv[])
{

	UNUSED(argc);
	UNUSED(argv);
	WINDOW *topl, *topr, *botl, *botr;
	
	cout << "Curses interface under construction ! " << endl;
	initscr();
	noecho();

	topl = subwin(stdscr, LINES / 2 , COLS / 2, 0, 0);
	topr = subwin(stdscr, LINES / 2 , COLS / 2, 0, COLS / 2);
	botl = subwin(stdscr, LINES / 2 , COLS / 2, LINES /2 , 0);
	botr = subwin(stdscr, LINES / 2 , COLS / 2, LINES /2, COLS/2);

	box(topl, 0, ACS_HLINE);
	box(topr, 0, ACS_HLINE);
	box(botl, 0, ACS_HLINE);
	box(botr, 0, ACS_HLINE);


	mvwprintw(topl, 1, 1, "TOP LEFT");
	mvwprintw(topr, 1, 1, "TOP RIGHT");
	mvwprintw(botl, 1, 1, "BOT LEFT");
	mvwprintw(botr, 1, 1, "BOT RIGHT");
	
	wrefresh(topl);
	wrefresh(topr);
	wrefresh(botl);
	wrefresh(botr);

	getch();
	endwin();
	return 0;
}
