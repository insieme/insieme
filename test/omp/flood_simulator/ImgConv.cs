using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.IO;

namespace ImgConv
{
    class ImgConv
    {
        static void Main(string[] args)
        {
            using (StreamWriter outfile = new StreamWriter(args[0] + ".map"))
            {
                Image img = Image.FromFile(args[0]);
                Bitmap bmp = new Bitmap(img);
                int size = img.Height < img.Width ? img.Height : img.Width;
                outfile.WriteLine(size);
                for (int y = 0; y < size; ++y)
                {
                    for (int x = 0; x < size; ++x)
                    {
                        outfile.Write((int)(bmp.GetPixel(x, y).GetBrightness() * 10));
                        outfile.Write(" ");
                    }
                    outfile.WriteLine();
                }
            }
        }
    }
}
