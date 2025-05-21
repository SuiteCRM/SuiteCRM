<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser;

use ZBateson\MailMimeParser\Message\PartHeaderContainer;

/**
 * Reads headers from an input stream, adding them to a PartHeaderContainer.
 *
 * @author Zaahid Bateson
 */
class HeaderParser
{
    /**
     * Ensures the header isn't empty and contains a colon separator character,
     * then splits it and adds it to the passed PartHeaderContainer.
     *
     * @param string $header the header line
     * @param PartHeaderContainer $headerContainer the container
     */
    private function addRawHeaderToPart(string $header, PartHeaderContainer $headerContainer) : self
    {
        if ($header !== '' && \strpos($header, ':') !== false) {
            $a = \explode(':', $header, 2);
            $headerContainer->add($a[0], \trim($a[1]));
        }
        return $this;
    }

    /**
     * Reads header lines up to an empty line, adding them to the passed
     * PartHeaderContainer.
     *
     * @param resource $handle The resource handle to read from.
     * @param PartHeaderContainer $container the container to add headers to.
     */
    public function parse($handle, PartHeaderContainer $container) : self
    {
        $header = '';
        do {
            $line = MessageParser::readLine($handle);
            if ($line === false || $line === '' || $line[0] !== "\t" && $line[0] !== ' ') {
                $this->addRawHeaderToPart($header, $container);
                $header = '';
            } else {
                $line = "\r\n" . $line;
            }
            $header .= \rtrim($line, "\r\n");
        } while ($header !== '');
        return $this;
    }
}
