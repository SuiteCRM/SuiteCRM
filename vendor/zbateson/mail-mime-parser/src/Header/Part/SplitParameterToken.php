<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Part;

use ZBateson\MbWrapper\MbWrapper;

/**
 * Holds a running value for an RFC-2231 split header parameter.
 *
 * ParameterConsumer creates SplitParameterTokens when a split header parameter
 * is first found, and adds subsequent split parts to an already created one if
 * the parameter name matches.
 *
 * @author Zaahid Bateson
 */
class SplitParameterToken extends HeaderPart
{
    /**
     * @var string name of the parameter.
     */
    protected $name;

    /**
     * @var string[] keeps encoded parts values that need to be decoded.  Keys
     *      are set to the index part of the split parameter and used for
     *      sorting before decoding/concatenating.
     */
    protected $encodedParts = [];

    /**
     * @var string[] contains literal parts that don't require any decoding (and
     *      are therefore ISO-8859-1 (technically should be 7bit US-ASCII but
     *      allowing 8bit shouldn't be an issue as elsewhere in MMP).
     */
    protected $literalParts = [];

    /**
     * @var string RFC-1766 (or subset) language code with optional subtags,
     *      regions, etc...
     */
    protected $language;

    /**
     * @var string charset of content in $encodedParts.
     */
    protected $charset = 'ISO-8859-1';

    /**
     * Initializes a SplitParameterToken.
     *
     * @param string $name the parameter's name
     */
    public function __construct(MbWrapper $charsetConverter, $name)
    {
        parent::__construct($charsetConverter);
        $this->name = \trim($name);
    }

    /**
     * Extracts charset and language from an encoded value, setting them on the
     * current object if $index is 0 and adds the value part to the encodedParts
     * array.
     */
    protected function extractMetaInformationAndValue(string $value, int $index) : self
    {
        if (\preg_match('~^([^\']*)\'([^\']*)\'(.*)$~', $value, $matches)) {
            if ($index === 0) {
                $this->charset = (!empty($matches[1])) ? $matches[1] : $this->charset;
                $this->language = (!empty($matches[2])) ? $matches[2] : $this->language;
            }
            $value = $matches[3];
        }
        $this->encodedParts[$index] = $value;
        return $this;
    }

    /**
     * Adds the passed part to the running array of values.
     *
     * If $isEncoded is true, language and charset info is extracted from the
     * value, and the value is decoded before returning in getValue.
     *
     * The value of the parameter is sorted based on the passed $index
     * arguments when adding before concatenating when re-constructing the
     * value.
     *
     * @param string $value
     * @param bool $isEncoded
     * @param int $index
     */
    public function addPart($value, $isEncoded, $index)
    {
        if (empty($index)) {
            $index = 0;
        }
        if ($isEncoded) {
            $this->extractMetaInformationAndValue($value, $index);
        } else {
            $this->literalParts[$index] = $this->convertEncoding($value);
        }
    }

    /**
     * Traverses $this->encodedParts until a non-sequential key is found, or the
     * end of the array is found.
     *
     * This allows encoded parts of a split parameter to be split anywhere and
     * reconstructed.
     *
     * The returned string is converted to UTF-8 before being returned.
     *
     */
    private function getNextEncodedValue() : string
    {
        $cur = \current($this->encodedParts);
        $key = \key($this->encodedParts);
        $running = '';
        while ($cur !== false) {
            $running .= $cur;
            $cur = \next($this->encodedParts);
            $nKey = \key($this->encodedParts);
            if ($nKey !== $key + 1) {
                break;
            }
            $key = $nKey;
        }
        return $this->convertEncoding(
            \rawurldecode($running),
            $this->charset,
            true
        );
    }

    /**
     * Reconstructs the value of the split parameter into a single UTF-8 string
     * and returns it.
     */
    public function getValue() : ?string
    {
        $parts = $this->literalParts;

        \reset($this->encodedParts);
        \ksort($this->encodedParts);
        while (\current($this->encodedParts) !== false) {
            $parts[\key($this->encodedParts)] = $this->getNextEncodedValue();
        }

        \ksort($parts);
        return \array_reduce(
            $parts,
            function($carry, $item) {
                return $carry . $item;
            },
            ''
        );
    }

    /**
     * Returns the name of the parameter.
     *
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * Returns the language of the parameter if set, or null if not.
     *
     * @return string
     */
    public function getLanguage()
    {
        return $this->language;
    }
}
