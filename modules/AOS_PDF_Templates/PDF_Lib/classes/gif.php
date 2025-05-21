<?php
///////////////////////////////////////////////////////////////////////////////////////////////////
// 2009-12-22 Adapted for mPDF 4.2
///////////////////////////////////////////////////////////////////////////////////////////////////
// GIF Util - (C) 2003 Yamasoft (S/C)
// http://www.yamasoft.com
// All Rights Reserved
// This file can be freely copied, distributed, modified, updated by anyone under the only
// condition to leave the original address (Yamasoft, http://www.yamasoft.com) and this header.
///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
// 2009-12-22 Adapted INB
// Functions calling functionname($x, $len = 0) were not working on PHP5.1.5 as pass by reference
// All edited to $len = 0; then call function.
///////////////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////////////

class CGIFLZW
{
    public $MAX_LZW_BITS;
    public $Fresh;
    public $CodeSize;
    public $SetCodeSize;
    public $MaxCode;
    public $MaxCodeSize;
    public $FirstCode;
    public $OldCode;
    public $ClearCode;
    public $EndCode;
    public $Next;
    public $Vals;
    public $Stack;
    public $sp;
    public $Buf;
    public $CurBit;
    public $LastBit;
    public $Done;
    public $LastByte;

    ///////////////////////////////////////////////////////////////////////////

    // CONSTRUCTOR
    public function __construct()
    {
        $this->MAX_LZW_BITS = 12;
        unset($this->Next);
        unset($this->Vals);
        unset($this->Stack);
        unset($this->Buf);

        $this->Next  = range(0, (1 << $this->MAX_LZW_BITS)       - 1);
        $this->Vals  = range(0, (1 << $this->MAX_LZW_BITS)       - 1);
        $this->Stack = range(0, (1 << ($this->MAX_LZW_BITS + 1)) - 1);
        $this->Buf   = range(0, 279);
    }

    ///////////////////////////////////////////////////////////////////////////

    public function deCompress($data, &$datLen)
    {
        $stLen  = strlen($data);
        $datLen = 0;
        $ret    = "";
        $dp = 0; 	// data pointer

        // INITIALIZATION
        $this->LZWCommandInit($data, $dp);

        while (($iIndex = $this->LZWCommand($data, $dp)) >= 0) {
            $ret .= chr($iIndex);
        }

        $datLen = $dp;

        if ($iIndex != -2) {
            return false;
        }

        return $ret;
    }

    ///////////////////////////////////////////////////////////////////////////
    public function LZWCommandInit(&$data, &$dp)
    {
        $this->SetCodeSize = ord($data[0]);
        $dp += 1;

        $this->CodeSize    = $this->SetCodeSize + 1;
        $this->ClearCode   = 1 << $this->SetCodeSize;
        $this->EndCode     = $this->ClearCode + 1;
        $this->MaxCode     = $this->ClearCode + 2;
        $this->MaxCodeSize = $this->ClearCode << 1;

        $this->GetCodeInit($data, $dp);

        $this->Fresh = 1;
        for ($i = 0; $i < $this->ClearCode; $i++) {
            $this->Next[$i] = 0;
            $this->Vals[$i] = $i;
        }

        for (; $i < (1 << $this->MAX_LZW_BITS); $i++) {
            $this->Next[$i] = 0;
            $this->Vals[$i] = 0;
        }

        $this->sp = 0;
        return 1;
    }

    public function LZWCommand(&$data, &$dp)
    {
        if ($this->Fresh) {
            $this->Fresh = 0;
            do {
                $this->FirstCode = $this->GetCode($data, $dp);
                $this->OldCode   = $this->FirstCode;
            } while ($this->FirstCode == $this->ClearCode);

            return $this->FirstCode;
        }

        if ($this->sp > 0) {
            $this->sp--;
            return $this->Stack[$this->sp];
        }

        while (($Code = $this->GetCode($data, $dp)) >= 0) {
            if ($Code == $this->ClearCode) {
                for ($i = 0; $i < $this->ClearCode; $i++) {
                    $this->Next[$i] = 0;
                    $this->Vals[$i] = $i;
                }

                for (; $i < (1 << $this->MAX_LZW_BITS); $i++) {
                    $this->Next[$i] = 0;
                    $this->Vals[$i] = 0;
                }

                $this->CodeSize    = $this->SetCodeSize + 1;
                $this->MaxCodeSize = $this->ClearCode << 1;
                $this->MaxCode     = $this->ClearCode + 2;
                $this->sp          = 0;
                $this->FirstCode   = $this->GetCode($data, $dp);
                $this->OldCode     = $this->FirstCode;

                return $this->FirstCode;
            }

            if ($Code == $this->EndCode) {
                return -2;
            }

            $InCode = $Code;
            if ($Code >= $this->MaxCode) {
                $this->Stack[$this->sp++] = $this->FirstCode;
                $Code = $this->OldCode;
            }

            while ($Code >= $this->ClearCode) {
                $this->Stack[$this->sp++] = $this->Vals[$Code];

                if ($Code == $this->Next[$Code]) { // Circular table entry, big GIF Error!
                    return -1;
                }

                $Code = $this->Next[$Code];
            }

            $this->FirstCode = $this->Vals[$Code];
            $this->Stack[$this->sp++] = $this->FirstCode;

            if (($Code = $this->MaxCode) < (1 << $this->MAX_LZW_BITS)) {
                $this->Next[$Code] = $this->OldCode;
                $this->Vals[$Code] = $this->FirstCode;
                $this->MaxCode++;

                if (($this->MaxCode >= $this->MaxCodeSize) && ($this->MaxCodeSize < (1 << $this->MAX_LZW_BITS))) {
                    $this->MaxCodeSize *= 2;
                    $this->CodeSize++;
                }
            }

            $this->OldCode = $InCode;
            if ($this->sp > 0) {
                $this->sp--;
                return $this->Stack[$this->sp];
            }
        }

        return $Code;
    }

    ///////////////////////////////////////////////////////////////////////////

    public function GetCodeInit(&$data, &$dp)
    {
        $this->CurBit   = 0;
        $this->LastBit  = 0;
        $this->Done     = 0;
        $this->LastByte = 2;
        return 1;
    }

    public function GetCode(&$data, &$dp)
    {
        if (($this->CurBit + $this->CodeSize) >= $this->LastBit) {
            if ($this->Done) {
                if ($this->CurBit >= $this->LastBit) {
                    // Ran off the end of my bits
                    return 0;
                }
                return -1;
            }

            $this->Buf[0] = $this->Buf[$this->LastByte - 2];
            $this->Buf[1] = $this->Buf[$this->LastByte - 1];

            $Count = ord($data[$dp]);
            $dp += 1;

            if ($Count) {
                for ($i = 0; $i < $Count; $i++) {
                    $this->Buf[2 + $i] = ord($data[$dp+$i]);
                }
                $dp += $Count;
            } else {
                $this->Done = 1;
            }

            $this->LastByte = 2 + $Count;
            $this->CurBit   = ($this->CurBit - $this->LastBit) + 16;
            $this->LastBit  = (2 + $Count) << 3;
        }

        $iRet = 0;
        for ($i = $this->CurBit, $j = 0; $j < $this->CodeSize; $i++, $j++) {
            $iRet |= (($this->Buf[(int)($i / 8)] & (1 << ($i % 8))) != 0) << $j;
        }

        $this->CurBit += $this->CodeSize;
        return $iRet;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

class CGIFCOLORTABLE
{
    public $m_nColors;
    public $m_arColors;

    ///////////////////////////////////////////////////////////////////////////

    // CONSTRUCTOR
    public function __construct()
    {
        unset($this->m_nColors);
        unset($this->m_arColors);
    }

    ///////////////////////////////////////////////////////////////////////////

    public function load($lpData, $num)
    {
        $this->m_nColors  = 0;
        $this->m_arColors = array();

        for ($i = 0; $i < $num; $i++) {
            $rgb = substr($lpData, $i * 3, 3);
            if (strlen($rgb) < 3) {
                return false;
            }

            $this->m_arColors[] = (ord($rgb[2]) << 16) + (ord($rgb[1]) << 8) + ord($rgb[0]);
            $this->m_nColors++;
        }

        return true;
    }

    ///////////////////////////////////////////////////////////////////////////

    public function toString()
    {
        $ret = "";

        for ($i = 0; $i < $this->m_nColors; $i++) {
            $ret .=
                chr(($this->m_arColors[$i] & 0x000000FF))       . // R
                chr(($this->m_arColors[$i] & 0x0000FF00) >>  8) . // G
                chr(($this->m_arColors[$i] & 0x00FF0000) >> 16);  // B
        }

        return $ret;
    }


    ///////////////////////////////////////////////////////////////////////////

    public function colorIndex($rgb)
    {
        $rgb  = (int)$rgb & 0xFFFFFF;
        $r1   = ($rgb & 0x0000FF);
        $g1   = ($rgb & 0x00FF00) >>  8;
        $b1   = ($rgb & 0xFF0000) >> 16;
        $idx  = -1;

        for ($i = 0; $i < $this->m_nColors; $i++) {
            $r2 = ($this->m_arColors[$i] & 0x000000FF);
            $g2 = ($this->m_arColors[$i] & 0x0000FF00) >>  8;
            $b2 = ($this->m_arColors[$i] & 0x00FF0000) >> 16;
            $d  = abs($r2 - $r1) + abs($g2 - $g1) + abs($b2 - $b1);

            if (($idx == -1) || ($d < $dif)) {
                $idx = $i;
                $dif = $d;
            }
        }

        return $idx;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

class CGIFFILEHEADER
{
    public $m_lpVer;
    public $m_nWidth;
    public $m_nHeight;
    public $m_bGlobalClr;
    public $m_nColorRes;
    public $m_bSorted;
    public $m_nTableSize;
    public $m_nBgColor;
    public $m_nPixelRatio;
    public $m_colorTable;

    ///////////////////////////////////////////////////////////////////////////

    // CONSTRUCTOR
    public function __construct()
    {
        unset($this->m_lpVer);
        unset($this->m_nWidth);
        unset($this->m_nHeight);
        unset($this->m_bGlobalClr);
        unset($this->m_nColorRes);
        unset($this->m_bSorted);
        unset($this->m_nTableSize);
        unset($this->m_nBgColor);
        unset($this->m_nPixelRatio);
        unset($this->m_colorTable);
    }

    ///////////////////////////////////////////////////////////////////////////

    public function load($lpData, &$hdrLen)
    {
        $hdrLen = 0;

        $this->m_lpVer = substr($lpData, 0, 6);
        if (($this->m_lpVer <> "GIF87a") && ($this->m_lpVer <> "GIF89a")) {
            return false;
        }

        $this->m_nWidth  = $this->w2i(substr($lpData, 6, 2));
        $this->m_nHeight = $this->w2i(substr($lpData, 8, 2));
        if (!$this->m_nWidth || !$this->m_nHeight) {
            return false;
        }

        $b = ord(substr($lpData, 10, 1));
        $this->m_bGlobalClr  = ($b & 0x80) ? true : false;
        $this->m_nColorRes   = ($b & 0x70) >> 4;
        $this->m_bSorted     = ($b & 0x08) ? true : false;
        $this->m_nTableSize  = 2 << ($b & 0x07);
        $this->m_nBgColor    = ord(substr($lpData, 11, 1));
        $this->m_nPixelRatio = ord(substr($lpData, 12, 1));
        $hdrLen = 13;

        if ($this->m_bGlobalClr) {
            $this->m_colorTable = new CGIFCOLORTABLE();
            if (!$this->m_colorTable->load(substr($lpData, $hdrLen), $this->m_nTableSize)) {
                return false;
            }
            $hdrLen += 3 * $this->m_nTableSize;
        }

        return true;
    }

    ///////////////////////////////////////////////////////////////////////////

    public function w2i($str)
    {
        return ord(substr($str, 0, 1)) + (ord(substr($str, 1, 1)) << 8);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

class CGIFIMAGEHEADER
{
    public $m_nLeft;
    public $m_nTop;
    public $m_nWidth;
    public $m_nHeight;
    public $m_bLocalClr;
    public $m_bInterlace;
    public $m_bSorted;
    public $m_nTableSize;
    public $m_colorTable;

    ///////////////////////////////////////////////////////////////////////////

    // CONSTRUCTOR
    public function __construct()
    {
        unset($this->m_nLeft);
        unset($this->m_nTop);
        unset($this->m_nWidth);
        unset($this->m_nHeight);
        unset($this->m_bLocalClr);
        unset($this->m_bInterlace);
        unset($this->m_bSorted);
        unset($this->m_nTableSize);
        unset($this->m_colorTable);
    }

    ///////////////////////////////////////////////////////////////////////////

    public function load($lpData, &$hdrLen)
    {
        $hdrLen = 0;

        $this->m_nLeft   = $this->w2i(substr($lpData, 0, 2));
        $this->m_nTop    = $this->w2i(substr($lpData, 2, 2));
        $this->m_nWidth  = $this->w2i(substr($lpData, 4, 2));
        $this->m_nHeight = $this->w2i(substr($lpData, 6, 2));

        if (!$this->m_nWidth || !$this->m_nHeight) {
            return false;
        }

        $b = ord($lpData[8]);
        $this->m_bLocalClr  = ($b & 0x80) ? true : false;
        $this->m_bInterlace = ($b & 0x40) ? true : false;
        $this->m_bSorted    = ($b & 0x20) ? true : false;
        $this->m_nTableSize = 2 << ($b & 0x07);
        $hdrLen = 9;

        if ($this->m_bLocalClr) {
            $this->m_colorTable = new CGIFCOLORTABLE();
            if (!$this->m_colorTable->load(substr($lpData, $hdrLen), $this->m_nTableSize)) {
                return false;
            }
            $hdrLen += 3 * $this->m_nTableSize;
        }

        return true;
    }

    ///////////////////////////////////////////////////////////////////////////

    public function w2i($str)
    {
        return ord(substr($str, 0, 1)) + (ord(substr($str, 1, 1)) << 8);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

class CGIFIMAGE
{
    public $m_disp;
    public $m_bUser;
    public $m_bTrans;
    public $m_nDelay;
    public $m_nTrans;
    public $m_lpComm;
    public $m_gih;
    public $m_data;
    public $m_lzw;

    ///////////////////////////////////////////////////////////////////////////

    public function __construct()
    {
        unset($this->m_disp);
        unset($this->m_bUser);
        unset($this->m_bTrans);
        unset($this->m_nDelay);
        unset($this->m_nTrans);
        unset($this->m_lpComm);
        unset($this->m_data);
        $this->m_gih = new CGIFIMAGEHEADER();
        $this->m_lzw = new CGIFLZW();
    }

    ///////////////////////////////////////////////////////////////////////////

    public function load($data, &$datLen)
    {
        $datLen = 0;

        while (true) {
            $b = ord($data[0]);
            $data = substr($data, 1);
            $datLen++;

            switch ($b) {
            case 0x21: // Extension
                $len = 0;
                if (!$this->skipExt($data, $len)) {
                    return false;
                }
                $datLen += $len;
                break;

            case 0x2C: // Image
                // LOAD HEADER & COLOR TABLE
                $len = 0;
                if (!$this->m_gih->load($data, $len)) {
                    return false;
                }
                $data = substr($data, $len);
                $datLen += $len;

                // ALLOC BUFFER
                $len = 0;

                if (!($this->m_data = $this->m_lzw->deCompress($data, $len))) {
                    return false;
                }

                $data = substr($data, $len);
                $datLen += $len;

                if ($this->m_gih->m_bInterlace) {
                    $this->deInterlace();
                }

                return true;

            case 0x3B: // EOF
            default:
                return false;
            }
        }
        return false;
    }

    ///////////////////////////////////////////////////////////////////////////

    public function skipExt(&$data, &$extLen)
    {
        $extLen = 0;

        $b = ord($data[0]);
        $data = substr($data, 1);
        $extLen++;

        switch ($b) {
        case 0xF9: // Graphic Control
            $b = ord($data[1]);
            $this->m_disp   = ($b & 0x1C) >> 2;
            $this->m_bUser  = ($b & 0x02) ? true : false;
            $this->m_bTrans = ($b & 0x01) ? true : false;
            $this->m_nDelay = $this->w2i(substr($data, 2, 2));
            $this->m_nTrans = ord($data[4]);
            break;

        case 0xFE: // Comment
            $this->m_lpComm = substr($data, 1, ord($data[0]));
            break;

        case 0x01: // Plain text
            break;

        case 0xFF: // Application
            break;
        }

        // SKIP DEFAULT AS DEFS MAY CHANGE
        $b = ord($data[0]);
        $data = substr($data, 1);
        $extLen++;
        while ($b > 0) {
            $data = substr($data, $b);
            $extLen += $b;
            $b    = ord($data[0]);
            $data = substr($data, 1);
            $extLen++;
        }
        return true;
    }

    ///////////////////////////////////////////////////////////////////////////

    public function w2i($str)
    {
        return ord(substr($str, 0, 1)) + (ord(substr($str, 1, 1)) << 8);
    }

    ///////////////////////////////////////////////////////////////////////////

    public function deInterlace()
    {
        $data = $this->m_data;

        for ($i = 0; $i < 4; $i++) {
            switch ($i) {
            case 0:
                $s = 8;
                $y = 0;
                break;

            case 1:
                $s = 8;
                $y = 4;
                break;

            case 2:
                $s = 4;
                $y = 2;
                break;

            case 3:
                $s = 2;
                $y = 1;
                break;
            }

            for (; $y < $this->m_gih->m_nHeight; $y += $s) {
                $lne = substr($this->m_data, 0, $this->m_gih->m_nWidth);
                $this->m_data = substr($this->m_data, $this->m_gih->m_nWidth);

                $data =
                    substr($data, 0, $y * $this->m_gih->m_nWidth) .
                    $lne .
                    substr($data, ($y + 1) * $this->m_gih->m_nWidth);
            }
        }

        $this->m_data = $data;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////

class CGIF
{
    public $m_gfh;
    public $m_lpData;
    public $m_img;
    public $m_bLoaded;

    ///////////////////////////////////////////////////////////////////////////

    // CONSTRUCTOR
    public function __construct()
    {
        $this->m_gfh     = new CGIFFILEHEADER();
        $this->m_img     = new CGIFIMAGE();
        $this->m_lpData  = "";
        $this->m_bLoaded = false;
    }

    ///////////////////////////////////////////////////////////////////////////
    public function ClearData()
    {
        $this->m_lpData = '';
        unset($this->m_img->m_data);
        unset($this->m_img->m_lzw->Next);
        unset($this->m_img->m_lzw->Vals);
        unset($this->m_img->m_lzw->Stack);
        unset($this->m_img->m_lzw->Buf);
    }

    public function loadFile(&$data, $iIndex)
    {
        if ($iIndex < 0) {
            return false;
        }
        $this->m_lpData = $data;

        // GET FILE HEADER
        $len = 0;
        if (!$this->m_gfh->load($this->m_lpData, $len)) {
            return false;
        }

        $this->m_lpData = substr($this->m_lpData, $len);

        do {
            $imgLen = 0;
            if (!$this->m_img->load($this->m_lpData, $imgLen)) {
                return false;
            }
            $this->m_lpData = substr($this->m_lpData, $imgLen);
        } while ($iIndex-- > 0);

        $this->m_bLoaded = true;
        return true;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
