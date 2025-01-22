<?php

/**
 * @file
 * @deprecated since v7.12.0
 * Class PdfParser
 *
 * @author : Sebastien MALOT <sebastien@malot.fr>
 * @date : 2013-08-08
 *
 * References :
 * - http://www.mactech.com/articles/mactech/Vol.15/15.09/PDFIntro/index.html
 * - http://framework.zend.com/issues/secure/attachment/12512/Pdf.php
 * - http://www.php.net/manual/en/ref.pdf.php#74211
 */
#[\AllowDynamicProperties]
class PdfParser
{
    /**
     * Parse PDF file
     *
     * @deprecated since v7.12.0
     * @param string $filename
     * @return string
     */
    public static function parseFile($filename)
    {
        $content = file_get_contents($filename);

        return self::extractText($content);
    }

    /**
     * Parse PDF content
     *
     * @deprecated since v7.12.0
     * @param string $content
     * @return string
     */
    public static function parseContent($content)
    {
        return self::extractText($content);
    }

    /**
     * Convert a PDF into text.
     *
     * @deprecated since v7.12.0
     * @param string $filename The filename to extract the data from.
     * @return string The extracted text from the PDF
     */
    protected static function extractText($data)
    {
        /**
         * Split apart the PDF document into sections. We will address each
         * section separately.
         */
        $a_obj    = self::getDataArray($data, 'obj', 'endobj');
        $j        = 0;
        $a_chunks = array();

        /**
         * Attempt to extract each part of the PDF document into a 'filter'
         * element and a 'data' element. This can then be used to decode the
         * data.
         */
        foreach ($a_obj as $obj) {
            $a_filter = self::getDataArray($obj, '<<', '>>');

            if (is_array($a_filter) && isset($a_filter[0])) {
                $a_chunks[$j]['filter'] = $a_filter[0];
                $a_data = self::getDataArray($obj, 'stream', 'endstream');

                if (is_array($a_data) && isset($a_data[0])) {
                    $a_chunks[$j]['data'] = trim(substr((string) $a_data[0], strlen('stream'), strlen((string) $a_data[0]) - strlen('stream') - strlen('endstream')));
                }

                $j++;
            }
        }

        $result_data = null;

        // decode the chunks
        foreach ($a_chunks as $chunk) {
            // Look at each chunk decide if we can decode it by looking at the contents of the filter
            if (isset($chunk['data'])) {

        // look at the filter to find out which encoding has been used
                if (strpos((string) $chunk['filter'], 'FlateDecode') !== false) {
                    // Use gzuncompress but suppress error messages.
                    $data =@ gzuncompress($chunk['data']);
                } else {
                    $data = $chunk['data'];
                }

                if (trim($data) != '') {
                    // If we got data then attempt to extract it.
                    $result_data .= ' ' . self::extractTextElements($data);
                }
            }
        }

        /**
         * Make sure we don't have large blocks of white space before and after
         * our string. Also extract alphanumerical information to reduce
         * redundant data.
         */
        if (trim($result_data) == '') {
            return null;
        } else {
            // Optimize hyphened words
            $result_data = preg_replace('/\s*-[\r\n]+\s*/', '', $result_data);
            $result_data = preg_replace('/\s+/', ' ', $result_data);

            return $result_data;
        }
    }

    /**
     * @deprecated since v7.12.0
     * @param $content
     * @return string|string[]
     */
    protected static function extractTextElements($content)
    {
        if (strpos((string) $content, '/CIDInit') === 0) {
            return '';
        }

        $text  = '';
        $lines = explode("\n", $content);

        foreach ($lines as $line) {
            $line = trim($line);
            $matches = array();

            // Parse each lines to extract command and operator values
            if (preg_match('/^(?<command>.*[\)\] ])(?<operator>[a-z]+[\*]?)$/i', $line, $matches)) {
                $command = trim($matches['command']);

                // Convert octal encoding
                $found_octal_values = array();
                preg_match_all('/\\\\([0-9]{3})/', $command, $found_octal_values);

                foreach ($found_octal_values[0] as $value) {
                    $octal = substr((string) $value, 1);

                    if ((int)$octal < 40) {
                        // Skips non printable chars
                        $command = str_replace($value, '', $command);
                    } else {
                        $command = str_replace($value, chr(octdec($octal)), $command);
                    }
                }
                // Removes encoded new lines, tabs, ...
                $command = preg_replace('/\\\\[\r\n]/', '', $command);
                $command = preg_replace('/\\\\[rnftb ]/', ' ', $command);
                // Force UTF-8 charset
                $encoding = mb_detect_encoding($command, array('ASCII', 'UTF-8', 'Windows-1252', 'ISO-8859-1'));
                if (strtoupper($encoding) != 'UTF-8') {
                    if ($decoded = @iconv('CP1252', 'UTF-8//TRANSLIT//IGNORE', $command)) {
                        $command = $decoded;
                    }
                }
                // Removes leading spaces
                $operator = trim($matches['operator']);
            } else {
                $command = $line;
                $operator = '';
            }

            // Handle main operators
            switch ($operator) {
        // Set character spacing.
        case 'Tc':
          break;

        // Move text current point.
        case 'Td':
          $values = explode(' ', $command);
          $y = array_pop($values);
          $x = array_pop($values);
          if ($x > 0) {
              $text .= ' ';
          }
          if ($y < 0) {
              $text .= ' ';
          }
          break;

        // Move text current point and set leading.
        case 'TD':
          $values = explode(' ', $command);
          $y = array_pop($values);
          if ($y < 0) {
              $text .= "\n";
          }
          break;

        // Set font name and size.
        case 'Tf':
          $text.= ' ';
          break;

        // Display text, allowing individual character positioning
        case 'TJ':
          $start = mb_strpos($command, '[', null, 'UTF-8') + 1;
          $end   = mb_strrpos($command, ']', null, 'UTF-8');
          $text.= self::parseTextCommand(mb_substr($command, $start, $end - $start, 'UTF-8'));
          break;

        // Display text.
        case 'Tj':
          $start = mb_strpos($command, '(', null, 'UTF-8') + 1;
          $end   = mb_strrpos($command, ')', null, 'UTF-8');
          $text.= mb_substr($command, $start, $end - $start, 'UTF-8'); // Removes round brackets
          break;

        // Set leading.
        case 'TL':

        // Set text matrix.
        case 'Tm':
//          $text.= ' ';
          break;

        // Set text rendering mode.
        case 'Tr':
          break;

        // Set super/subscripting text rise.
        case 'Ts':
          break;

        // Set text spacing.
        case 'Tw':
          break;

        // Set horizontal scaling.
        case 'Tz':
          break;

        // Move to start of next line.
        case 'T*':
          $text.= "\n";
          break;

        // Internal use
        case 'g':
        case 'gs':
        case 're':
        case 'f':
        // Begin text
        case 'BT':
        // End text
        case 'ET':
          break;

        case '':
          break;

        default:
      }
        }

        $text = str_replace(array('\\(', '\\)'), array('(', ')'), $text);

        return $text;
    }

    /**
     * Strip out the text from a small chunk of data.
     *
     * @deprecated since v7.12.0
     * @param string $text
     * @param int $font_size Currently not used
     *
     * @return string
     */
    protected static function parseTextCommand($text, $font_size = 0)
    {
        $result = '';
        $cur_start_pos = 0;

        while (($cur_start_text = mb_strpos($text, '(', $cur_start_pos, 'UTF-8')) !== false) {
            // New text element found
            if ($cur_start_text - $cur_start_pos > 8) {
                $spacing = ' ';
            } else {
                $spacing_size = mb_substr($text, $cur_start_pos, $cur_start_text - $cur_start_pos, 'UTF-8');

                if ($spacing_size < -50) {
                    $spacing = ' ';
                } else {
                    $spacing = '';
                }
            }
            $cur_start_text++;

            $start_search_end = $cur_start_text;
            while (($cur_start_pos = mb_strpos($text, ')', $start_search_end, 'UTF-8')) !== false) {
                if (mb_substr($text, $cur_start_pos - 1, 1, 'UTF-8') != '\\') {
                    break;
                }
                $start_search_end = $cur_start_pos + 1;
            }

            // something wrong happened
            if ($cur_start_pos === false) {
                break;
            }

            // Add to result
            $result .= $spacing . mb_substr($text, $cur_start_text, $cur_start_pos - $cur_start_text, 'UTF-8');
            $cur_start_pos++;
        }

        return $result;
    }

    /**
     * Convert a section of data into an array, separated by the start and end words.
     * @deprecated since v7.12.0
     * @param  string $data       The data.
     * @param  string $start_word The start of each section of data.
     * @param  string $end_word   The end of each section of data.
     * @return array              The array of data.
     */
    protected static function getDataArray($data, $start_word, $end_word)
    {
        $start     = 0;
        $end       = 0;
        $a_results = array();

        while ($start !== false && $end !== false) {
            $start = strpos($data, $start_word, $end);
            $end   = strpos($data, $end_word, $start);

            if ($end !== false && $start !== false) {
                // data is between start and end
                $a_results[] = substr($data, $start, $end - $start + strlen($end_word));
            }
        }

        return $a_results;
    }
}
