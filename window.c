/* Copyright 2011-2013 Bert Muennich
 *
 * This file is part of sxiv.
 *
 * sxiv is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * sxiv is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with sxiv.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "sxiv.h"
#define _WINDOW_CONFIG
#include "config.h"
#include "icon/data.h"
#include "utf8.h"

#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#define RES_CLASS "Sxiv"

enum {
	H_TEXT_PAD = 5,
	V_TEXT_PAD = 1
};

static struct {
	int name;
	Cursor icon;
} cursors[CURSOR_COUNT] = {
	{ XC_left_ptr }, { XC_dotbox }, { XC_watch },
	{ XC_sb_left_arrow }, { XC_sb_right_arrow }
};

static GC gc;

static XftFont *font;
static int fontheight;
static double fontsize;
static int barheight;

static XftFont *li_font;
static int li_fontheight;
static double li_fontsize;
static int li_barheight;


Atom atoms[ATOM_COUNT];

void win_clone_to_root(win_t *win){
	win_env_t *e;
	e = &win->env;
//try:
//RootWindow(e->dpy, e->scr);

//	win_t win = *winp;
//--test bg
	Window tswroot, tswparent;
	Window * tswchildren;
	unsigned int tswnchildren;
	XQueryTree(e->dpy, win->xwin, &tswroot, &tswparent, &tswchildren, &tswnchildren);
	XSetWindowBackgroundPixmap(e->dpy, tswroot, win->buf.pm);
//++test bg
}

void win_init_font(const win_env_t *e, const char *fontstr)
{
	if ((font = XftFontOpenName(e->dpy, e->scr, fontstr)) == NULL)
		error(EXIT_FAILURE, 0, "Error loading font '%s'", fontstr);
	fontheight = font->ascent + font->descent;
	FcPatternGetDouble(font->pattern, FC_SIZE, 0, &fontsize);
	barheight = fontheight + 2 * V_TEXT_PAD;
}

void win_init_font_li(const win_env_t *e, const char *fontstr)
{
	if ((li_font = XftFontOpenName(e->dpy, e->scr, fontstr)) == NULL)
		error(EXIT_FAILURE, 0, "Error loading font '%s'", fontstr);
	li_fontheight = li_font->ascent + li_font->descent;
	FcPatternGetDouble(li_font->pattern, FC_SIZE, 0, &li_fontsize);
	li_barheight = li_fontheight + 2 * V_TEXT_PAD;
}

void win_alloc_color(const win_env_t *e, const char *name, XftColor *col)
{
	if (!XftColorAllocName(e->dpy, DefaultVisual(e->dpy, e->scr),
	                       DefaultColormap(e->dpy, e->scr), name, col))
	{
		error(EXIT_FAILURE, 0, "Error allocating color '%s'", name);
	}
}

const char* win_res(XrmDatabase db, const char *name, const char *def)
{
	char *type;
	XrmValue ret;

	if (db != None &&
	    XrmGetResource(db, name, name, &type, &ret) &&
	    STREQ(type, "String"))
	{
		return ret.addr;
	} else {
		return def;
	}
}

#define INIT_ATOM_(atom) \
	atoms[ATOM_##atom] = XInternAtom(e->dpy, #atom, False);

void win_init(win_t *win)
{
	win_env_t *e;
	const char *bg, *fg, *f, *f2;
	char *res_man;
	XrmDatabase db;

	memset(win, 0, sizeof(win_t));

	e = &win->env;
	if ((e->dpy = XOpenDisplay(NULL)) == NULL)
		error(EXIT_FAILURE, 0, "Error opening X display");

	e->scr = DefaultScreen(e->dpy);
	e->scrw = DisplayWidth(e->dpy, e->scr);
	e->scrh = DisplayHeight(e->dpy, e->scr);
	e->vis = DefaultVisual(e->dpy, e->scr);
	e->cmap = DefaultColormap(e->dpy, e->scr);
	e->depth = DefaultDepth(e->dpy, e->scr);

	if (setlocale(LC_CTYPE, "") == NULL || XSupportsLocale() == 0)
		error(0, 0, "No locale support");

	XrmInitialize();
	res_man = XResourceManagerString(e->dpy);
	db = res_man != NULL ? XrmGetStringDatabase(res_man) : None;

	f = win_res(db, RES_CLASS ".font", "monospace-8");
	f2 = win_res(db, RES_CLASS ".font-title", "monospace-20");
	win_init_font(e, f);
	win_init_font_li(e, f2);

	bg = win_res(db, RES_CLASS ".background", "white");
	fg = win_res(db, RES_CLASS ".foreground", "black");
	win_alloc_color(e, bg, &win->bg);
	win_alloc_color(e, fg, &win->fg);

	win->bar.l.size = BAR_L_LEN;
	win->bar.r.size = BAR_R_LEN;
	/* 3 padding bytes needed by utf8_decode */
	win->bar.l.buf = emalloc(win->bar.l.size + 3);
	win->bar.l.buf[0] = '\0';
	win->bar.r.buf = emalloc(win->bar.r.size + 3);
	win->bar.r.buf[0] = '\0';
	win->bar.h = options->hide_bar ? 0 : barheight;

	INIT_ATOM_(WM_DELETE_WINDOW);
	INIT_ATOM_(_NET_WM_NAME);
	INIT_ATOM_(_NET_WM_ICON_NAME);
	INIT_ATOM_(_NET_WM_ICON);
	INIT_ATOM_(_NET_WM_STATE);
	INIT_ATOM_(_NET_WM_STATE_FULLSCREEN);
}

void win_open(win_t *win)
{
	int c, i, j, n;
	long parent;
	win_env_t *e;
	XClassHint classhint;
	unsigned long *icon_data;
	XColor col;
	Cursor *cnone = &cursors[CURSOR_NONE].icon;
	char none_data[] = { 0, 0, 0, 0, 0, 0, 0, 0 };
	Pixmap none;
	int gmask;
	XSizeHints sizehints;

	e = &win->env;
	parent = options->embed != 0 ? options->embed : RootWindow(e->dpy, e->scr);

	sizehints.flags = PWinGravity;
	sizehints.win_gravity = NorthWestGravity;

	/* determine window offsets, width & height */
	if (options->geometry == NULL)
		gmask = 0;
	else
		gmask = XParseGeometry(options->geometry, &win->x, &win->y,
		                       &win->w, &win->h);
	if ((gmask & WidthValue) != 0)
		sizehints.flags |= USSize;
	else
		win->w = WIN_WIDTH;
	if ((gmask & HeightValue) != 0)
		sizehints.flags |= USSize;
	else
		win->h = WIN_HEIGHT;
	if ((gmask & XValue) != 0) {
		if ((gmask & XNegative) != 0) {
			win->x += e->scrw - win->w;
			sizehints.win_gravity = NorthEastGravity;
		}
		sizehints.flags |= USPosition;
	} else {
		win->x = 0;
	}
	if ((gmask & YValue) != 0) {
		if ((gmask & YNegative) != 0) {
			win->y += e->scrh - win->h;
			sizehints.win_gravity = sizehints.win_gravity == NorthEastGravity
			                      ? SouthEastGravity : SouthWestGravity;
		}
		sizehints.flags |= USPosition;
	} else {
		win->y = 0;
	}

	win->xwin = XCreateWindow(e->dpy, parent,
	                          win->x, win->y, win->w, win->h, 0,
	                          e->depth, InputOutput, e->vis, 0, NULL);
	if (win->xwin == None)
		error(EXIT_FAILURE, 0, "Error creating X window");

	XSelectInput(e->dpy, win->xwin,
	             ButtonReleaseMask | ButtonPressMask | KeyPressMask |
	             PointerMotionMask | StructureNotifyMask);

	for (i = 0; i < ARRLEN(cursors); i++) {
		if (i != CURSOR_NONE)
			cursors[i].icon = XCreateFontCursor(e->dpy, cursors[i].name);
	}
	if (XAllocNamedColor(e->dpy, DefaultColormap(e->dpy, e->scr), "black",
	                     &col, &col) == 0)
	{
		error(EXIT_FAILURE, 0, "Error allocating color 'black'");
	}
	none = XCreateBitmapFromData(e->dpy, win->xwin, none_data, 8, 8);
	*cnone = XCreatePixmapCursor(e->dpy, none, none, &col, &col, 0, 0);

	gc = XCreateGC(e->dpy, win->xwin, 0, None);

	n = icons[ARRLEN(icons)-1].size;
	icon_data = emalloc((n * n + 2) * sizeof(*icon_data));

	for (i = 0; i < ARRLEN(icons); i++) {
		n = 0;
		icon_data[n++] = icons[i].size;
		icon_data[n++] = icons[i].size;

		for (j = 0; j < icons[i].cnt; j++) {
			for (c = icons[i].data[j] >> 4; c >= 0; c--)
				icon_data[n++] = icon_colors[icons[i].data[j] & 0x0F];
		}
		XChangeProperty(e->dpy, win->xwin,
		                atoms[ATOM__NET_WM_ICON], XA_CARDINAL, 32,
		                i == 0 ? PropModeReplace : PropModeAppend,
		                (unsigned char *) icon_data, n);
	}
	free(icon_data);

	win_set_title(win, "sxiv");

	classhint.res_class = RES_CLASS;
	classhint.res_name = options->res_name != NULL ? options->res_name : "sxiv";
	XSetClassHint(e->dpy, win->xwin, &classhint);

	XSetWMProtocols(e->dpy, win->xwin, &atoms[ATOM_WM_DELETE_WINDOW], 1);

	sizehints.width = win->w;
	sizehints.height = win->h;
	sizehints.x = win->x;
	sizehints.y = win->y;
	XSetWMNormalHints(win->env.dpy, win->xwin, &sizehints);

	win->h -= win->bar.h;

	win->buf.w = e->scrw;
	win->buf.h = e->scrh;
	win->buf.pm = XCreatePixmap(e->dpy, win->xwin,
	                            win->buf.w, win->buf.h, e->depth);
	XSetForeground(e->dpy, gc, win->bg.pixel);
	XFillRectangle(e->dpy, win->buf.pm, gc, 0, 0, win->buf.w, win->buf.h);
	XSetWindowBackgroundPixmap(e->dpy, win->xwin, win->buf.pm);

	XMapWindow(e->dpy, win->xwin);
	XFlush(e->dpy);

	if (options->fullscreen)
		win_toggle_fullscreen(win);
}

CLEANUP void win_close(win_t *win)
{
	int i;

	for (i = 0; i < ARRLEN(cursors); i++)
		XFreeCursor(win->env.dpy, cursors[i].icon);

	XFreeGC(win->env.dpy, gc);

	XDestroyWindow(win->env.dpy, win->xwin);
	XCloseDisplay(win->env.dpy);
}

bool win_configure(win_t *win, XConfigureEvent *c)
{
	bool changed;

	changed = win->w != c->width || win->h + win->bar.h != c->height;

	win->x = c->x;
	win->y = c->y;
	win->w = c->width;
	win->h = c->height - win->bar.h;
	win->bw = c->border_width;

	return changed;
}

void win_toggle_fullscreen(win_t *win)
{
	XEvent ev;
	XClientMessageEvent *cm;

	memset(&ev, 0, sizeof(ev));
	ev.type = ClientMessage;

	cm = &ev.xclient;
	cm->window = win->xwin;
	cm->message_type = atoms[ATOM__NET_WM_STATE];
	cm->format = 32;
	cm->data.l[0] = 2; // toggle
	cm->data.l[1] = atoms[ATOM__NET_WM_STATE_FULLSCREEN];

	XSendEvent(win->env.dpy, DefaultRootWindow(win->env.dpy), False,
	           SubstructureNotifyMask | SubstructureRedirectMask, &ev);
}

void win_toggle_bar(win_t *win)
{
	if (win->bar.h != 0) {
		win->h += win->bar.h;
		win->bar.h = 0;
	} else {
		win->bar.h = barheight;
		win->h -= win->bar.h;
	}
}

void win_clear(win_t *win)
{
	win_env_t *e;

	e = &win->env;

	if (win->w > win->buf.w || win->h + win->bar.h > win->buf.h) {
		XFreePixmap(e->dpy, win->buf.pm);
		win->buf.w = MAX(win->buf.w, win->w);
		win->buf.h = MAX(win->buf.h, win->h + win->bar.h);
		win->buf.pm = XCreatePixmap(e->dpy, win->xwin,
		                            win->buf.w, win->buf.h, e->depth);
	}
	XSetForeground(e->dpy, gc, win->bg.pixel);
	XFillRectangle(e->dpy, win->buf.pm, gc, 0, 0, win->buf.w, win->buf.h);
}

#define TEXTWIDTH(win, text, len) \
	win_draw_text(win, NULL, NULL, 0, 0, text, len, 0)

#define TEXTWIDTHF(win, text, len, fnt) \
	win_draw_text_font(win, NULL, NULL, 0, 0, text, len, 0, fnt)


int win_draw_text_font(win_t *win, XftDraw *d, const XftColor *color, int x, int y,
                  char *text, int len, int w, XftFont *fin)
{
	int err, tw = 0;
	char *t, *next;
	uint32_t rune;
	XftFont *f;
	FcCharSet *fccharset;
	XGlyphInfo ext;

	for (t = text; t - text < len; t = next) {
		next = utf8_decode(t, &rune, &err);
		if (XftCharExists(win->env.dpy, fin, rune)) {
			f = fin;
		} else { /* fallback font */
			fccharset = FcCharSetCreate();
			FcCharSetAddChar(fccharset, rune);
			f = XftFontOpen(win->env.dpy, win->env.scr, FC_CHARSET, FcTypeCharSet,
			                fccharset, FC_SCALABLE, FcTypeBool, FcTrue,
			                FC_SIZE, FcTypeDouble, fontsize, NULL);
			FcCharSetDestroy(fccharset);
		}
		XftTextExtentsUtf8(win->env.dpy, f, (XftChar8*)t, next - t, &ext);
		tw += ext.xOff;
		if (tw <= w) {
			XftDrawStringUtf8(d, color, f, x, y, (XftChar8*)t, next - t);
			x += ext.xOff;
		}
		if (f != fin)
			XftFontClose(win->env.dpy, f);
	}
	return tw;
}

int win_draw_text(win_t *win, XftDraw *d, const XftColor *color, int x, int y,
                  char *text, int len, int w)
{
	return win_draw_text_font(win,d,color,x,y,text,len,w,font);
}

void win_draw_bar(win_t *win)
{
	win_bar_t *l, *r;

	if ((l = &win->bar.l)->buf == NULL || (r = &win->bar.r)->buf == NULL)
		return;
	//default:
	win_draw_bar_at(win,l,r,0,win->w,win->h,win->bar.h,false);
	//alt:
	//border:
	//win_draw_bar_at(win, l,r,10, win->w-20, win->h-10, win->bar.h);
	//center right:
	//win_draw_bar_at(win,l, r, (win->w/2), (win->w/2)-10, (win->h/2)-(win->bar.h/2), win->bar.h);

	//split:
	//win_draw_bar_at(win,l,NULL,0,win->w,win->h,win->bar.h);
	//win_draw_bar_at(win,NULL, r, (win->w/2), (win->w/2)-10, (win->h/2)-(win->bar.h/2), win->bar.h);

}

void win_draw_bar_at(win_t *win, win_bar_t *l, win_bar_t *r, int barx, int barw, int bary, int barh, bool thumb)
{
	int len, x, y, w, tw;
	win_env_t *e;
	XftDraw *d;
	XftFont *fu;
//new: use win_draw_text_font (selected font last param)

	e = &win->env;
	if (thumb){
		fu=li_font;
		barh=li_barheight;
		//fu=font;
	} else {
		fu=font;
	}
	y = bary + fu->ascent + V_TEXT_PAD;
	w = barw - 2*H_TEXT_PAD;
	d = XftDrawCreate(e->dpy, win->buf.pm, DefaultVisual(e->dpy, e->scr),
	                  DefaultColormap(e->dpy, e->scr));

	XSetForeground(e->dpy, gc, win->fg.pixel);
	XFillRectangle(e->dpy, win->buf.pm, gc, barx, bary, barw, barh);

	XSetForeground(e->dpy, gc, win->bg.pixel);
	XSetBackground(e->dpy, gc, win->fg.pixel);

	if(r != NULL){
		if ((len = strlen(r->buf)) > 0) {
			if ((tw = TEXTWIDTHF(win, r->buf, len, fu)) > w)
				return;
			x = barw + barx - tw - H_TEXT_PAD;
			w -= tw;
			win_draw_text_font(win, d, &win->bg, x, y+barh, r->buf, len, tw, fu);
		}
	}
	if(l != NULL){
		if ((len = strlen(l->buf)) > 0) {
			x = barx + H_TEXT_PAD;
			w -= 2 * H_TEXT_PAD; /* gap between left and right parts */
			win_draw_text_font(win, d, &win->bg, x, y, l->buf, len, w, fu);
			if ((tw = TEXTWIDTHF(win, l->buf, len, fu)) > w) //test
				if(thumb){
					//win_draw_bar_at(win, l, NULL, barx, barw, bary+barh, barh, false);
					char * newstr;
					//newstr = l->buf + (len * sizeof(char));
					while(TEXTWIDTHF(win, l->buf, len, fu)>w){
						len--;
						newstr = l->buf + (len * sizeof(char));
					}
					win_bar_t xbt1;
					win_bar_t * xbt = &xbt1;
					xbt->buf = newstr;
					win_draw_bar_at(win, xbt, NULL, barx, barw, bary+barh, barh, true);
				}
				//win_draw_bar_at(win, l, r, barx, barw, bary+barh, barh, false);
				//win_draw_text_font(win, d, &win->bg, x, y, l->buf, len, w, fu);
		}
	}
	XftDrawDestroy(d);
}

void win_draw(win_t *win)
{
	if (win->bar.h > 0)
		win_draw_bar(win);

	XSetWindowBackgroundPixmap(win->env.dpy, win->xwin, win->buf.pm);
	XClearWindow(win->env.dpy, win->xwin);
	XFlush(win->env.dpy);
}

void win_draw_rect(win_t *win, int x, int y, int w, int h, bool fill, int lw,
                   unsigned long col)
{
	XGCValues gcval;

	gcval.line_width = lw;
	gcval.foreground = col;
	XChangeGC(win->env.dpy, gc, GCForeground | GCLineWidth, &gcval);

	if (fill)
		XFillRectangle(win->env.dpy, win->buf.pm, gc, x, y, w, h);
	else
		XDrawRectangle(win->env.dpy, win->buf.pm, gc, x, y, w, h);
}

void win_set_title(win_t *win, const char *title)
{
	XStoreName(win->env.dpy, win->xwin, title);
	XSetIconName(win->env.dpy, win->xwin, title);

	XChangeProperty(win->env.dpy, win->xwin, atoms[ATOM__NET_WM_NAME],
	                XInternAtom(win->env.dpy, "UTF8_STRING", False), 8,
	                PropModeReplace, (unsigned char *) title, strlen(title));
	XChangeProperty(win->env.dpy, win->xwin, atoms[ATOM__NET_WM_ICON_NAME],
	                XInternAtom(win->env.dpy, "UTF8_STRING", False), 8,
	                PropModeReplace, (unsigned char *) title, strlen(title));
}

void win_set_cursor(win_t *win, cursor_t cursor)
{
	if (cursor >= 0 && cursor < ARRLEN(cursors)) {
		XDefineCursor(win->env.dpy, win->xwin, cursors[cursor].icon);
		XFlush(win->env.dpy);
	}
}

void win_cursor_pos(win_t *win, int *x, int *y)
{
	int i;
	unsigned int ui;
	Window w;

	if (!XQueryPointer(win->env.dpy, win->xwin, &w, &w, &i, &i, x, y, &ui))
		*x = *y = 0;
}

