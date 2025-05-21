<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Part;

use ZBateson\MbWrapper\MbWrapper;

/**
 * Represents a single mime header part token, with the possibility of it being
 * MIME-Encoded as per RFC-2047.
 *
 * MimeLiteralPart automatically decodes the value if it's encoded.
 *
 * @author Zaahid Bateson
 */
class MimeLiteralPart extends LiteralPart
{
    /**
     * @var string regex pattern matching a mime-encoded part
     */
    public const MIME_PART_PATTERN = '=\?[^?=]+\?[QBqb]\?[^\?]+\?=';

    /**
     * @var string regex pattern used when parsing parameterized headers
     */
    public const MIME_PART_PATTERN_NO_QUOTES = '=\?[^\?=]+\?[QBqb]\?[^\?"]+\?=';

    /**
     * @var bool set to true to ignore spaces before this part
     */
    protected $canIgnoreSpacesBefore = false;

    /**
     * @var bool set to true to ignore spaces after this part
     */
    protected $canIgnoreSpacesAfter = false;

    /**
     * @var array maintains an array mapping rfc1766 language tags to parts of
     * text in the value.
     *
     * Each array element is an array containing two elements, one with key
     * 'lang', and another with key 'value'.
     */
    protected $languages = [];

    /**
     * Decoding the passed token value if it's mime-encoded and assigns the
     * decoded value to a member variable. Sets canIgnoreSpacesBefore and
     * canIgnoreSpacesAfter.
     *
     * @param string $token
     */
    public function __construct(MbWrapper $charsetConverter, $token)
    {
        parent::__construct($charsetConverter);
        $this->value = $this->decodeMime($token);
        // preg_match returns int
        $pattern = self::MIME_PART_PATTERN;
        $this->canIgnoreSpacesBefore = (bool) \preg_match("/^\s*{$pattern}/", $token);
        $this->canIgnoreSpacesAfter = (bool) \preg_match("/{$pattern}\s*\$/", $token);
    }

    /**
     * Finds and replaces mime parts with their values.
     *
     * The method splits the token value into an array on mime-part-patterns,
     * either replacing a mime part with its value by calling iconv_mime_decode
     * or converts the encoding on the text part by calling convertEncoding.
     *
     */
    protected function decodeMime(string $value) : string
    {
        $pattern = self::MIME_PART_PATTERN;
        // remove whitespace between two adjacent mime encoded parts
        $value = \preg_replace("/($pattern)\\s+(?=$pattern)/", '$1', $value);
        // with PREG_SPLIT_DELIM_CAPTURE, matched and unmatched parts are returned
        $aMimeParts = \preg_split("/($pattern)/", $value, -1, PREG_SPLIT_DELIM_CAPTURE | PREG_SPLIT_NO_EMPTY);
        $ret = '';
        foreach ($aMimeParts as $entity) {
            $ret .= $this->decodeSplitPart($entity);
        }
        return $ret;
    }

    /**
     * Decodes a matched mime entity part into a string and returns it, after
     * adding the string into the languages array.
     *
     * @param string[] $matches
     */
    private function decodeMatchedEntity(array $matches) : string
    {
        $body = $matches[4];
        if (\strtoupper($matches[3]) === 'Q') {
            $body = \quoted_printable_decode(\str_replace('_', '=20', $body));
        } else {
            $body = \base64_decode($body);
        }
        $language = $matches[2];
        $decoded = $this->convertEncoding($body, $matches[1], true);
        $this->addToLanguage($decoded, $language);
        return $decoded;
    }

    /**
     * Decodes a single mime-encoded entity.
     *
     * Unfortunately, mb_decode_header fails for many charsets on PHP 5.4 and
     * PHP 5.5 (even if they're listed as supported).  iconv_mime_decode doesn't
     * support all charsets.
     *
     * Parsing out the charset and body of the encoded entity seems to be the
     * way to go to support the most charsets.
     *
     */
    private function decodeSplitPart(string $entity) : string
    {
        if (\preg_match('/^=\?([A-Za-z\-_0-9]+)\*?([A-Za-z\-_0-9]+)?\?([QBqb])\?([^\?]*)\?=$/', $entity, $matches)) {
            return $this->decodeMatchedEntity($matches);
        }
        $decoded = $this->convertEncoding($entity);
        $this->addToLanguage($decoded);
        return $decoded;
    }

    /**
     * Returns true if spaces before this part should be ignored.
     *
     * Overridden to return $this->canIgnoreSpacesBefore which is setup in the
     * constructor.
     *
     */
    public function ignoreSpacesBefore() : bool
    {
        return $this->canIgnoreSpacesBefore;
    }

    /**
     * Returns true if spaces before this part should be ignored.
     *
     * Overridden to return $this->canIgnoreSpacesAfter which is setup in the
     * constructor.
     *
     */
    public function ignoreSpacesAfter() : bool
    {
        return $this->canIgnoreSpacesAfter;
    }

    /**
     * Adds the passed part into the languages array with the given language.
     */
    protected function addToLanguage(string $part, ?string $language = null) : self
    {
        $this->languages[] = [
            'lang' => $language,
            'value' => $part
        ];
        return $this;
    }

    /**
     * Returns an array of parts mapped to languages in the header value, for
     * instance the string:
     *
     * 'Hello and =?UTF-8*fr-be?Q?bonjour_?= =?UTF-8*it?Q?mi amici?=. Welcome!'
     *
     * Would be mapped in the returned array as follows:
     *
     * ```php
     * [
     *     0 => [ 'lang' => null, 'value' => 'Hello and ' ],
     *     1 => [ 'lang' => 'fr-be', 'value' => 'bonjour ' ],
     *     3 => [ 'lang' => 'it', 'value' => 'mi amici' ],
     *     4 => [ 'lang' => null, 'value' => ' Welcome!' ]
     * ]
     * ```
     *
     * @return string[][]
     */
    public function getLanguageArray()
    {
        return $this->languages;
    }
}
