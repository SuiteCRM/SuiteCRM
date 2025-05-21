<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer;

use ArrayObject;
use ZBateson\MailMimeParser\Header\IHeaderPart;
use ZBateson\MailMimeParser\Header\Part\MimeLiteralPart;
use ZBateson\MailMimeParser\Header\Part\SplitParameterToken;
use ZBateson\MailMimeParser\Header\Part\Token;

/**
 * Reads headers separated into parameters consisting of an optional main value,
 * and subsequent name/value pairs - for example text/html; charset=utf-8.
 *
 * A ParameterConsumer's parts are separated by a semi-colon.  Its name/value
 * pairs are separated with an '=' character.
 *
 * Parts may be mime-encoded entities.  Additionally, a value can be quoted and
 * comments may exist.
 *
 * @author Zaahid Bateson
 */
class ParameterConsumer extends GenericConsumer
{
    /**
     * Returns semi-colon and equals char as token separators.
     *
     * @return string[]
     */
    protected function getTokenSeparators() : array
    {
        return [';', '='];
    }

    /**
     * Overridden to use a specialized regex for finding mime-encoded parts
     * (RFC 2047).
     *
     * Some implementations seem to place mime-encoded parts within quoted
     * parameters, and split the mime-encoded parts across multiple split
     * parameters.  The specialized regex doesn't allow double quotes inside a
     * mime encoded part, so it can be "continued" in another parameter.
     *
     * @return string the regex pattern
     */
    protected function getTokenSplitPattern() : string
    {
        $sChars = \implode('|', $this->getAllTokenSeparators());
        $mimePartPattern = MimeLiteralPart::MIME_PART_PATTERN_NO_QUOTES;
        return '~(' . $mimePartPattern . '|\\\\.|' . $sChars . ')~';
    }

    /**
     * Creates and returns a \ZBateson\MailMimeParser\Header\Part\Token out of
     * the passed string token and returns it, unless the token is an escaped
     * literal, in which case a LiteralPart is returned.
     *
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart
     */
    protected function getPartForToken(string $token, bool $isLiteral)
    {
        if ($isLiteral) {
            return $this->partFactory->newLiteralPart($token);
        }
        return $this->partFactory->newToken($token);
    }

    /**
     * Adds the passed parameter with the given name and value to a
     * SplitParameterToken, at the passed index. If one with the given name
     * doesn't exist, it is created.
     *
     * @return SplitParameterToken
     */
    private function addToSplitPart(ArrayObject $splitParts, string $name, string $value, int $index, bool $isEncoded)
    {
        $ret = null;
        if (!isset($splitParts[$name])) {
            $ret = $this->partFactory->newSplitParameterToken($name);
            $splitParts[$name] = $ret;
        }
        $splitParts[$name]->addPart($value, $isEncoded, $index);
        return $ret;
    }

    /**
     * Instantiates and returns either a MimeLiteralPart if $strName is empty,
     * a SplitParameterToken if the parameter is a split parameter and is the
     * first in a series, null if it's a split parameter but is not the first
     * part in its series, or a ParameterPart is returned otherwise.
     *
     * If the part is a SplitParameterToken, it's added to the passed
     * $splitParts as well with its name as a key.
     *
     * @return MimeLiteralPart|SplitParameterToken|\ZBateson\MailMimeParser\Header\Part\ParameterPart
     */
    private function getPartFor(string $strName, string $strValue, ArrayObject $splitParts)
    {
        if ($strName === '') {
            return $this->partFactory->newMimeLiteralPart($strValue);
        } elseif (\preg_match('~^\s*([^\*]+)\*(\d*)(\*)?$~', $strName, $matches)) {
            return $this->addToSplitPart(
                $splitParts,
                $matches[1],
                $strValue,
                (int) $matches[2],
                (($matches[2] === '') || !empty($matches[3]))
            );
        }
        return $this->partFactory->newParameterPart($strName, $strValue);
    }

    /**
     * Handles parameter separator tokens during final processing.
     *
     * If the end token is found, a new IHeaderPart is assigned to the passed
     * $combined array.  If an '=' character is found, $strCat is assigned to
     * $strName and emptied.
     *
     * Returns true if the token was processed, and false otherwise.
     *
     */
    private function processTokenPart(string $tokenValue, ArrayObject $combined, ArrayObject $splitParts, string &$strName, string &$strCat) : bool
    {
        if ($tokenValue === ';') {
            $combined[] = $this->getPartFor($strName, $strCat, $splitParts);
            $strName = '';
            $strCat = '';
            return true;
        } elseif ($tokenValue === '=' && $strCat !== '') {
            $strName = $strCat;
            $strCat = '';
            return true;
        }
        return false;
    }

    /**
     * Loops over parts in the passed array, creating ParameterParts out of any
     * parsed SplitParameterTokens, replacing them in the array.
     *
     * The method then calls filterIgnoreSpaces to filter out empty elements in
     * the combined array and returns an array.
     *
     * @return IHeaderPart[]|array
     */
    private function finalizeParameterParts(ArrayObject $combined) : array
    {
        foreach ($combined as $key => $part) {
            if ($part instanceof SplitParameterToken) {
                $combined[$key] = $this->partFactory->newParameterPart(
                    $part->getName(),
                    $part->getValue(),
                    $part->getLanguage()
                );
            }
        }
        return $this->filterIgnoredSpaces($combined->getArrayCopy());
    }

    /**
     * Post processing involves creating Part\LiteralPart or Part\ParameterPart
     * objects out of created Token and LiteralParts.
     *
     * @param IHeaderPart[] $parts The parsed parts.
     * @return IHeaderPart[] Array of resulting final parts.
     */
    protected function processParts(array $parts) : array
    {
        $combined = new ArrayObject();
        $splitParts = new ArrayObject();
        $strCat = '';
        $strName = '';
        $parts[] = $this->partFactory->newToken(';');
        foreach ($parts as $part) {
            $pValue = $part->getValue();
            if ($part instanceof Token && $this->processTokenPart($pValue, $combined, $splitParts, $strName, $strCat)) {
                continue;
            }
            $strCat .= $pValue;
        }
        return $this->finalizeParameterParts($combined);
    }
}
