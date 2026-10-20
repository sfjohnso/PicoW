"""Small helpers over python-docx for the DM42 guide."""
import re
from docx import Document
from docx.enum.section import WD_ORIENT
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_BREAK
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt, RGBColor

NAVY = RGBColor(0x1F, 0x3A, 0x5F)
GREY = RGBColor(0x55, 0x55, 0x55)


def shade(cell_or_par_pr, fill):
    shd = OxmlElement("w:shd")
    shd.set(qn("w:val"), "clear"); shd.set(qn("w:color"), "auto"); shd.set(qn("w:fill"), fill)
    cell_or_par_pr.append(shd)


class Guide:
    def __init__(self):
        d = self.d = Document()
        s = d.sections[0]
        s.page_width, s.page_height = Inches(8.5), Inches(11)
        s.left_margin = s.right_margin = Inches(1); s.top_margin = s.bottom_margin = Inches(0.9)
        st = d.styles
        st["Normal"].font.name = "Calibri"; st["Normal"].font.size = Pt(11)
        st["Normal"]._element.rPr.rFonts.set(qn("w:eastAsia"), "Calibri")
        st["Normal"].paragraph_format.space_after = Pt(6)
        st["Normal"].paragraph_format.line_spacing = 1.08
        for name, size, before, after in (("Heading 1", 20, 18, 8), ("Heading 2", 14, 14, 4), ("Heading 3", 12, 10, 3)):
            h = st[name]; h.font.name = "Calibri"; h.font.size = Pt(size); h.font.bold = True; h.font.color.rgb = NAVY
            h._element.rPr.rFonts.set(qn("w:asciiTheme"), "minorHAnsi") if False else None
            h.paragraph_format.space_before = Pt(before); h.paragraph_format.space_after = Pt(after)
            h.paragraph_format.keep_with_next = True
        self.fig_n = 0
        self.tab_n = 0

    # -- inline markup: **bold**, `code`, _italic_ ---------------------------------
    def runs(self, par, text, size=None, color=None):
        for tok in re.split(r"(`.+?`|\*\*.+?\*\*|(?<![\w*])\*[^*\s][^*]*?\*(?![\w*]))", text):
            if not tok: continue
            if tok.startswith("**"):
                r = par.add_run(tok[2:-2]); r.bold = True
            elif tok.startswith("`"):
                r = par.add_run(tok[1:-1]); r.font.name = "Consolas"; r.font.size = Pt((size or 11) - 1)
                r._element.rPr.rFonts.set(qn("w:eastAsia"), "Consolas")
                rp = r._element.get_or_add_rPr(); shade(rp, "EFEFEF")
                continue
            elif len(tok) > 2 and tok[0] == "*" and tok[-1] == "*":
                r = par.add_run(tok[1:-1]); r.italic = True
            else:
                r = par.add_run(tok)
            if size: r.font.size = Pt(size)
            if color: r.font.color.rgb = color

    def h1(self, t, newpage=True):
        if newpage: self.d.add_page_break()
        return self.d.add_heading(t, 1)

    def h2(self, t): return self.d.add_heading(t.replace("`", ""), 2)
    def h3(self, t): return self.d.add_heading(t, 3)

    def p(self, t, italic=False, align=None, size=None, color=None, keep=False):
        par = self.d.add_paragraph(); self.runs(par, t, size, color)
        if italic:
            for r in par.runs: r.italic = True
        if align: par.alignment = align
        if keep: par.paragraph_format.keep_with_next = True
        return par

    def bullets(self, items, style="List Bullet"):
        for t in items:
            par = self.d.add_paragraph(style=style); self.runs(par, t)
            par.paragraph_format.space_after = Pt(2)

    def numbered(self, items):
        for n, t in enumerate(items, 1):
            par = self.d.add_paragraph(); par.paragraph_format.left_indent = Inches(0.35)
            par.paragraph_format.first_line_indent = Inches(-0.25); par.paragraph_format.space_after = Pt(2)
            self.runs(par, f"{n}.  " + t)

    def code(self, text, caption=None):
        t = self.d.add_table(rows=1, cols=1); t.alignment = WD_TABLE_ALIGNMENT.CENTER
        c = t.rows[0].cells[0]; shade(c._tc.get_or_add_tcPr(), "F3F3F3")
        c.paragraphs[0].text = ""
        lines = text.strip("\n").split("\n")
        for i, ln in enumerate(lines):
            par = c.paragraphs[0] if i == 0 else c.add_paragraph()
            par.paragraph_format.space_after = Pt(0); par.paragraph_format.line_spacing = 1.0
            r = par.add_run(ln if ln else " "); r.font.name = "Consolas"; r.font.size = Pt(8.5)
            r._element.rPr.rFonts.set(qn("w:eastAsia"), "Consolas")
        self._borders(t, "BBBBBB")
        if caption:
            cp = self.d.add_paragraph(); self.runs(cp, caption, 9, GREY); cp.paragraph_format.space_after = Pt(8)
        else:
            self.d.add_paragraph().paragraph_format.space_after = Pt(2)

    def _borders(self, tbl, color="999999"):
        tblPr = tbl._tbl.tblPr
        b = OxmlElement("w:tblBorders")
        for edge in ("top", "left", "bottom", "right", "insideH", "insideV"):
            e = OxmlElement(f"w:{edge}"); e.set(qn("w:val"), "single"); e.set(qn("w:sz"), "4"); e.set(qn("w:color"), color); b.append(e)
        tblPr.append(b)

    def table(self, header, rows, widths=None, caption=None, size=9.5, align_right_from=None):
        self.tab_n += 1
        if caption:
            cp = self.d.add_paragraph(); self.runs(cp, f"**Table {self.tab_n}.** {caption}", 9.5, GREY)
            cp.paragraph_format.keep_with_next = True; cp.paragraph_format.space_after = Pt(2)
        t = self.d.add_table(rows=1, cols=len(header)); t.alignment = WD_TABLE_ALIGNMENT.CENTER
        self._borders(t)
        for i, h in enumerate(header):
            c = t.rows[0].cells[i]; shade(c._tc.get_or_add_tcPr(), "1F3A5F")
            c.paragraphs[0].text = ""; r = c.paragraphs[0].add_run(h); r.bold = True; r.font.size = Pt(size); r.font.color.rgb = RGBColor(255, 255, 255)
        for ri, row in enumerate(rows):
            cells = t.add_row().cells
            for i, v in enumerate(row):
                cells[i].paragraphs[0].text = ""; self.runs(cells[i].paragraphs[0], str(v), size)
                cells[i].paragraphs[0].paragraph_format.space_after = Pt(0)
                if align_right_from is not None and i >= align_right_from:
                    cells[i].paragraphs[0].alignment = WD_ALIGN_PARAGRAPH.RIGHT
                if ri % 2: shade(cells[i]._tc.get_or_add_tcPr(), "F2F6FA")
        if widths:
            for row in t.rows:
                for i, w in enumerate(widths): row.cells[i].width = Inches(w)
        # repeat header row
        trPr = t.rows[0]._tr.get_or_add_trPr(); th = OxmlElement("w:tblHeader"); th.set(qn("w:val"), "true"); trPr.append(th)
        self.d.add_paragraph().paragraph_format.space_after = Pt(2)

    def box(self, title, items, fill="FFF4D6"):
        t = self.d.add_table(rows=1, cols=1); t.alignment = WD_TABLE_ALIGNMENT.CENTER
        c = t.rows[0].cells[0]; shade(c._tc.get_or_add_tcPr(), fill)
        self._borders(t, "C9A227")
        par = c.paragraphs[0]; r = par.add_run(title); r.bold = True; r.font.size = Pt(10.5)
        par.paragraph_format.space_after = Pt(2)
        for it in items:
            q = c.add_paragraph(); q.paragraph_format.space_after = Pt(1); q.paragraph_format.left_indent = Inches(0.15)
            self.runs(q, "• " + it, 10)
        self.d.add_paragraph().paragraph_format.space_after = Pt(2)

    def fig(self, path, caption, width=5.5):
        self.fig_n += 1
        par = self.d.add_paragraph(); par.alignment = WD_ALIGN_PARAGRAPH.CENTER
        par.paragraph_format.keep_with_next = True; par.paragraph_format.space_after = Pt(2)
        par.add_run().add_picture(path, width=Inches(width))
        cp = self.d.add_paragraph(); cp.alignment = WD_ALIGN_PARAGRAPH.CENTER
        self.runs(cp, f"**Figure {self.fig_n}.** {caption}", 9.5, GREY); cp.paragraph_format.space_after = Pt(10)

    def figs(self, items, caption, width=3.0):
        """Several pictures side by side with one caption. items = [path, ...]"""
        self.fig_n += 1
        t = self.d.add_table(rows=1, cols=len(items)); t.alignment = WD_TABLE_ALIGNMENT.CENTER
        for i, pth in enumerate(items):
            par = t.rows[0].cells[i].paragraphs[0]; par.alignment = WD_ALIGN_PARAGRAPH.CENTER
            par.add_run().add_picture(pth, width=Inches(width))
            par.paragraph_format.keep_with_next = True
        cp = self.d.add_paragraph(); cp.alignment = WD_ALIGN_PARAGRAPH.CENTER
        self.runs(cp, f"**Figure {self.fig_n}.** {caption}", 9.5, GREY); cp.paragraph_format.space_after = Pt(10)

    def toc(self):
        par = self.d.add_paragraph()
        r = par.add_run()
        for typ, txt in (("begin", None), (None, 'TOC \\o "1-2" \\h \\z \\u'), ("separate", None), (None, "Right-click and choose Update Field to build the table of contents."), ("end", None)):
            if typ:
                fc = OxmlElement("w:fldChar"); fc.set(qn("w:fldCharType"), typ); r._r.append(fc)
            elif txt.startswith("TOC"):
                it = OxmlElement("w:instrText"); it.set(qn("xml:space"), "preserve"); it.text = txt; r._r.append(it)
            else:
                t = OxmlElement("w:t"); t.text = txt; r._r.append(t)

    def footer_numbers(self, text):
        sec = self.d.sections[0]
        par = sec.footer.paragraphs[0]; par.alignment = WD_ALIGN_PARAGRAPH.CENTER
        r = par.add_run(text + "   |   page "); r.font.size = Pt(9); r.font.color.rgb = GREY
        r2 = par.add_run(); r2.font.size = Pt(9)
        for typ, txt in (("begin", None), (None, "PAGE"), ("end", None)):
            if typ:
                fc = OxmlElement("w:fldChar"); fc.set(qn("w:fldCharType"), typ); r2._r.append(fc)
            else:
                it = OxmlElement("w:instrText"); it.set(qn("xml:space"), "preserve"); it.text = txt; r2._r.append(it)

    def save(self, path): self.d.save(path)
