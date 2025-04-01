<?php
 /**
 *
 *
 * @package
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility Ltd <support@salesagility.com>
 */



/**
 * @deprecated since v7.12.0
 * @param $revisionId
 * @return string
 */
function getDocumentRevisionPath($revisionId)
{
    return "upload/$revisionId";
}

/**
 * Given a path to a PPTX document returns a lucene document with filename and contents set.
 * @deprecated since v7.12.0
 * @param $path
 * @return Zend_Search_Lucene_Document
 */
function createPPTXDocument($path)
{
    $doc = Zend_Search_Lucene_Document_Pptx::loadPptxFile($path);
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    return $doc;
}

/**
 * Given a path to a XLSX document returns a lucene document with filename and contents set.
 * @deprecated since v7.12.0
 * @param $path
 * @return Zend_Search_Lucene_Document
 */
function createXLSXDocument($path)
{
    $doc = Zend_Search_Lucene_Document_Xlsx::loadXlsxFile($path);
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    return $doc;
}
/**
 * Given a path to a HTML document returns a lucene document with filename and contents set.
 * @deprecated since v7.12.0
 * @param $path
 * @return Zend_Search_Lucene_Document
 */
function createHTMLDocument($path)
{
    $doc = Zend_Search_Lucene_Document_Html::loadHTMLFile($path);
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    return $doc;
}

/**
 * Given a path to a DocX document returns a lucene document with filename and contents set.
 * @deprecated since v7.12.0
 * @param $path
 * @return Zend_Search_Lucene_Document
 */
function createDocXDocument($path)
{
    $doc = Zend_Search_Lucene_Document_Docx::loadDocxFile($path);
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    return $doc;
}

/**
 * Given a path to a Doc document returns a lucene document with filename and contents set.
 * @deprecated since v7.12.0
 * @param $path
 * @return Zend_Search_Lucene_Document
 */
function createDocDocument($path)
{
    $fileHandle = fopen($path, 'rb');
    $line = @fread($fileHandle, filesize($path));
    $lines = explode(chr(0x0D), $line);
    $outtext = "";
    foreach ($lines as $thisline) {
        $pos = strpos($thisline, chr(0x00));
        if (($pos !== false)||(strlen($thisline)==0)) {
        } else {
            $outtext .= $thisline." ";
        }
    }
    $outtext = preg_replace("/[^a-zA-Z0-9\s\,\.\-\n\r\t@\/\_\(\)]/", "", $outtext);

    $doc = new Zend_Search_Lucene_Document();
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    $doc->addField(Zend_Search_Lucene_Field::UnStored('contents', $outtext));
    fclose($fileHandle);
    return $doc;
}

/**
 * Given a path to a PDF document returns a lucene document with filename and contents set.
 * @deprecated since v7.12.0
 * @param $path
 * @return Zend_Search_Lucene_Document
 */
function createPDFDocument($path)
{
    require_once('PdfParser.php');
    $text = PdfParser::parseFile($path);
    $doc = new Zend_Search_Lucene_Document();
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    $doc->addField(Zend_Search_Lucene_Field::UnStored('contents', $text));
    return $doc;
}

/**
 * Given a path to an ODT doc returns a lucene document with contents and filename set.
 * @deprecated since v7.12.0
 * @param $path
 * @return bool|Zend_Search_Lucene_Document
 */
function createOdtDocument($path)
{
    if (!is_file($path)) {
        return false;
    }
    $doc = new Zend_Search_Lucene_Document();
    $documentBody = array();
    $coreProperties = array();
    $package = new ZipArchive();
    $package->open($path);
    $contents = simplexml_load_string($package->getFromName("content.xml"));
    $paragraphs = $contents->xpath('//text:*');
    foreach ($paragraphs as $paragraph) {
        $documentBody[] = (string)$paragraph;
        $documentBody[] = ' ';
    }
    // Close file
    $package->close();
    $doc->addField(Zend_Search_Lucene_Field::UnStored('contents', implode(' ', $documentBody), 'UTF-8'));
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    return $doc;
}

/**
 * Given a path to a plain text doc returns a lucene document with $filename and $contents set appropriately.
 * @deprecated since v7.12.0
 * @param $path
 * @return Zend_Search_Lucene_Document
 */
function createTextDocument($path)
{
    $doc = new Zend_Search_Lucene_Document();
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    $doc->addField(Zend_Search_Lucene_Field::UnStored('contents', file_get_contents($path)));
    return $doc;
}


/**
 * Given the path to an rtf document returns a lucene document with $filename and $contents set appropriately.
 * @deprecated since v7.12.0
 * @param $path
 * @return Zend_Search_Lucene_Document
 */
function createRTFDocument($path)
{
    $doc = new Zend_Search_Lucene_Document();
    $doc->addField(Zend_Search_Lucene_Field::Text('filename', basename((string) $path)));
    $contents = rtf2text($path);
    //print_r($contents);
    $doc->addField(Zend_Search_Lucene_Field::UnStored('contents', $contents));
    return $doc;
}

/**
 * @deprecated since v7.12.0
 * @param $s
 * @return bool
 */
function rtf_isPlainText($s)
{
    $arrfailAt = array("*", "fonttbl", "colortbl", "datastore", "themedata");
    $arrfailAtCount = count($arrfailAt);
    for ($i = 0; $i < $arrfailAtCount; $i++) {
        if (!empty($s[$arrfailAt[$i]])) {
            return false;
        }
    }
    return true;
}

/**
 * @deprecated since v7.12.0
 * @param $filename
 * @return string
 */
function rtf2text($filename)
{
    // Read the data from the input file.
    $text = file_get_contents($filename);
    if (!strlen($text)) {
        return "";
    }

    // Create empty stack array.
    $document = "";
    $stack = array();
    $j = -1;
    // Read the data character-by- character…
    for ($i = 0, $len = strlen($text); $i < $len; $i++) {
        $c = $text[$i];

        // Depending on current character select the further actions.
        switch ($c) {
            // the most important key word backslash
            case "\\":
                // read next character
                $nc = $text[$i + 1];

                // If it is another backslash or nonbreaking space or hyphen,
                // then the character is plain text and add it to the output stream.
                if ($nc == '\\' && rtf_isPlainText($stack[$j])) {
                    $document .= '\\';
                } elseif ($nc == '~' && rtf_isPlainText($stack[$j])) {
                    $document .= ' ';
                } elseif ($nc == '_' && rtf_isPlainText($stack[$j])) {
                    $document .= '-';
                }
                // If it is an asterisk mark, add it to the stack.
                elseif ($nc == '*') {
                    $stack[$j]["*"] = true;
                }
                // If it is a single quote, read next two characters that are the hexadecimal notation
                // of a character we should add to the output stream.
                elseif ($nc == "'") {
                    $hex = substr($text, $i + 2, 2);
                    if (rtf_isPlainText($stack[$j])) {
                        $document .= html_entity_decode("&#".hexdec($hex).";");
                    }
                    //Shift the pointer.
                    $i += 2;
                // Since, we’ve found the alphabetic character, the next characters are control word
                    // and, possibly, some digit parameter.
                } elseif ($nc >= 'a' && $nc <= 'z' || $nc >= 'A' && $nc <= 'Z') {
                    $word = "";
                    $param = null;

                    // Start reading characters after the backslash.
                    for ($k = $i + 1, $m = 0; $k < strlen($text); $k++, $m++) {
                        $nc = $text[$k];
                        // If the current character is a letter and there were no digits before it,
                        // then we’re still reading the control word. If there were digits, we should stop
                        // since we reach the end of the control word.
                        if ($nc >= 'a' && $nc <= 'z' || $nc >= 'A' && $nc <= 'Z') {
                            if (empty($param)) {
                                $word .= $nc;
                            } else {
                                break;
                            }
                            // If it is a digit, store the parameter.
                        } elseif ($nc >= '0' && $nc <= '9') {
                            $param .= $nc;
                        }
                        // Since minus sign may occur only before a digit parameter, check whether
                        // $param is empty. Otherwise, we reach the end of the control word.
                        elseif ($nc == '-') {
                            if (empty($param)) {
                                $param .= $nc;
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    // Shift the pointer on the number of read characters.
                    $i += $m - 1;

                    // Start analyzing what we’ve read. We are interested mostly in control words.
                    $toText = "";
                    switch (strtolower($word)) {
                        // If the control word is "u", then its parameter is the decimal notation of the
                        // Unicode character that should be added to the output stream.
                        // We need to check whether the stack contains \ucN control word. If it does,
                        // we should remove the N characters from the output stream.
                        case "u":
                            $toText .= html_entity_decode("&#x".dechex($param).";");
                            $ucDelta = @$stack[$j]["uc"];
                            if ($ucDelta > 0) {
                                $i += $ucDelta;
                            }
                            break;
                        // Select line feeds, spaces and tabs.
                        case "par": case "page": case "column": case "line": case "lbr":
                        $toText .= "\n";
                        break;
                        case "emspace": case "enspace": case "qmspace":
                        $toText .= " ";
                        break;
                        case "tab": $toText .= "\t"; break;
                        // Add current date and time instead of corresponding labels.
                        case "chdate": $toText .= date("m.d.Y"); break;
                        case "chdpl": $toText .= date("l, j F Y"); break;
                        case "chdpa": $toText .= date("D, j M Y"); break;
                        case "chtime": $toText .= date("H:i:s"); break;
                        // Replace some reserved characters to their html analogs.
                        case "emdash": $toText .= html_entity_decode("&mdash;"); break;
                        case "endash": $toText .= html_entity_decode("&ndash;"); break;
                        case "bullet": $toText .= html_entity_decode("&#149;"); break;
                        case "lquote": $toText .= html_entity_decode("&lsquo;"); break;
                        case "rquote": $toText .= html_entity_decode("&rsquo;"); break;
                        case "ldblquote": $toText .= html_entity_decode("&laquo;"); break;
                        case "rdblquote": $toText .= html_entity_decode("&raquo;"); break;
                        // Add all other to the control words stack. If a control word
                        // does not include parameters, set &param to true.
                        default:
                            $stack[$j][strtolower($word)] = empty($param) ? true : $param;
                            break;
                    }
                    // Add data to the output stream if required.
                    if (rtf_isPlainText($stack[$j])) {
                        $document .= $toText;
                    }
                }

                $i++;
                break;
            // If we read the opening brace {, then new subgroup starts and we add
            // new array stack element and write the data from previous stack element to it.
            case "{":
                array_push($stack, $stack[$j++]);
                break;
            // If we read the closing brace }, then we reach the end of subgroup and should remove
            // the last stack element.
            case "}":
                array_pop($stack);
                $j--;
                break;
            // Skip “trash”.
            case '\0': case '\r': case '\f': case '\n': break;
            // Add other data to the output stream if required.
            default:
                if (rtf_isPlainText($stack[$j])) {
                    $document .= $c;
                }
                break;
        }
    }
    // Return result.
    return $document;
}
