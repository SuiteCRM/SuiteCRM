<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer;

use ArrayIterator;
use Iterator;
use NoRewindIterator;
use ZBateson\MailMimeParser\Header\Part\HeaderPartFactory;
use ZBateson\MailMimeParser\Header\Part\MimeLiteralPart;

/**
 * Abstract base class for all header token consumers.
 *
 * Defines the base parser that loops over tokens, consuming them and creating
 * header parts.
 *
 * @author Zaahid Bateson
 */
abstract class AbstractConsumer
{
    /**
     * @var ConsumerService used to get consumer instances for sub-consumers.
     */
    protected $consumerService;

    /**
     * @var HeaderPartFactory used to construct IHeaderPart objects
     */
    protected $partFactory;

    public function __construct(ConsumerService $consumerService, HeaderPartFactory $partFactory)
    {
        $this->consumerService = $consumerService;
        $this->partFactory = $partFactory;
    }

    /**
     * Returns the singleton instance for the class.
     *
     */
    public static function getInstance(ConsumerService $consumerService, HeaderPartFactory $partFactory)
    {
        static $instances = [];
        $class = static::class;
        if (!isset($instances[$class])) {
            $instances[$class] = new static($consumerService, $partFactory);
        }
        return $instances[$class];
    }

    /**
     * Invokes parsing of a header's value into header parts.
     *
     * @param string $value the raw header value
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[] the array of parsed
     *         parts
     */
    public function __invoke(string $value) : array
    {
        if ($value !== '') {
            return $this->parseRawValue($value);
        }
        return [];
    }

    /**
     * Returns an array of sub-consumers.
     *
     * Called during construction to set up the list of sub-consumers that will
     * take control from this consumer should a token match a sub-consumer's
     * start token.
     *
     * @return AbstractConsumer[] Array of sub-consumers
     */
    abstract protected function getSubConsumers() : array;

    /**
     * Returns this consumer and all unique sub consumers.
     *
     * Loops into the sub-consumers (and their sub-consumers, etc...) finding
     * all unique consumers, and returns them in an array.
     *
     * @return AbstractConsumer[] Array of unique consumers.
     */
    protected function getAllConsumers() : array
    {
        $found = [$this];
        do {
            $current = \current($found);
            $subConsumers = $current->getSubConsumers();
            foreach ($subConsumers as $consumer) {
                if (!\in_array($consumer, $found)) {
                    $found[] = $consumer;
                }
            }
        } while (\next($found) !== false);
        return $found;
    }

    /**
     * Parses the raw header value into header parts.
     *
     * Calls splitTokens to split the value into token part strings, then calls
     * parseParts to parse the returned array.
     *
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[] the array of parsed
     *         parts
     */
    private function parseRawValue(string $value) : array
    {
        $tokens = $this->splitRawValue($value);
        return $this->parseTokensIntoParts(new NoRewindIterator(new ArrayIterator($tokens)));
    }

    /**
     * Returns an array of regular expression separators specific to this
     * consumer.
     *
     * The returned patterns are used to split the header value into tokens for
     * the consumer to parse into parts.
     *
     * Each array element makes part of a generated regular expression that is
     * used in a call to preg_split().  RegEx patterns can be used, and care
     * should be taken to escape special characters.
     *
     * @return string[] Array of regex patterns.
     */
    abstract protected function getTokenSeparators() : array;

    /**
     * Returns a list of regular expression markers for this consumer and all
     * sub-consumers by calling getTokenSeparators().
     *
     * @return string[] Array of regular expression markers.
     */
    protected function getAllTokenSeparators() : array
    {
        $markers = $this->getTokenSeparators();
        $subConsumers = $this->getAllConsumers();
        foreach ($subConsumers as $consumer) {
            $markers = \array_merge($consumer->getTokenSeparators(), $markers);
        }
        return \array_unique($markers);
    }

    /**
     * Returns a regex pattern used to split the input header string.
     *
     * The default implementation calls
     * {@see AbstractConsumer::getAllTokenSeparators()} and implodes the
     * returned array with the regex OR '|' character as its glue.
     *
     * @return string the regex pattern
     */
    protected function getTokenSplitPattern() : string
    {
        $sChars = \implode('|', $this->getAllTokenSeparators());
        $mimePartPattern = MimeLiteralPart::MIME_PART_PATTERN;
        return '~(' . $mimePartPattern . '|\\\\.|' . $sChars . ')~';
    }

    /**
     * Returns an array of split tokens from the input string.
     *
     * The method calls preg_split using
     * {@see AbstractConsumer::getTokenSplitPattern()}.  The split array will
     * not contain any empty parts and will contain the markers.
     *
     * @param string $rawValue the raw string
     * @return array the array of tokens
     */
    protected function splitRawValue($rawValue) : array
    {
        return \preg_split(
            $this->getTokenSplitPattern(),
            $rawValue,
            -1,
            PREG_SPLIT_DELIM_CAPTURE | PREG_SPLIT_NO_EMPTY
        );
    }

    /**
     * Returns true if the passed string token marks the beginning marker for
     * the current consumer.
     *
     * @param string $token The current token
     */
    abstract protected function isStartToken(string $token) : bool;

    /**
     * Returns true if the passed string token marks the end marker for the
     * current consumer.
     *
     * @param string $token The current token
     */
    abstract protected function isEndToken(string $token) : bool;

    /**
     * Constructs and returns an IHeaderPart for the passed string token.
     *
     * If the token should be ignored, the function must return null.
     *
     * The default created part uses the instance's partFactory->newInstance
     * method.
     *
     * @param string $token the token
     * @param bool $isLiteral set to true if the token represents a literal -
     *        e.g. an escaped token
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart|null The constructed
     *         header part or null if the token should be ignored.
     */
    protected function getPartForToken(string $token, bool $isLiteral)
    {
        if ($isLiteral) {
            return $this->partFactory->newLiteralPart($token);
        } elseif (\preg_match('/^\s+$/', $token)) {
            return $this->partFactory->newToken(' ');
        }
        return $this->partFactory->newInstance($token);
    }

    /**
     * Iterates through this consumer's sub-consumers checking if the current
     * token triggers a sub-consumer's start token and passes control onto that
     * sub-consumer's parseTokenIntoParts().
     *
     * If no sub-consumer is responsible for the current token, calls
     * {@see AbstractConsumer::getPartForToken()} and returns it in an array.
     *
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[]
     */
    protected function getConsumerTokenParts(Iterator $tokens) : array
    {
        $token = $tokens->current();
        $subConsumers = $this->getSubConsumers();
        foreach ($subConsumers as $consumer) {
            if ($consumer->isStartToken($token)) {
                $this->advanceToNextToken($tokens, true);
                return $consumer->parseTokensIntoParts($tokens);
            }
        }
        return [$this->getPartForToken($token, false)];
    }

    /**
     * Returns an array of IHeaderPart for the current token on the iterator.
     *
     * If the current token is a start token from a sub-consumer, the sub-
     * consumer's {@see AbstractConsumer::parseTokensIntoParts()} method is
     * called.
     *
     * @param Iterator $tokens The token iterator.
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[]
     */
    protected function getTokenParts(Iterator $tokens) : array
    {
        $token = $tokens->current();
        if (\strlen($token) === 2 && $token[0] === '\\') {
            return [$this->getPartForToken(\substr($token, 1), true)];
        }
        return $this->getConsumerTokenParts($tokens);
    }

    /**
     * Determines if the iterator should be advanced to the next token after
     * reading tokens or finding a start token.
     *
     * The default implementation will advance for a start token, but not
     * advance on the end token of the current consumer, allowing the end token
     * to be passed up to a higher-level consumer.
     *
     * @param Iterator $tokens The token iterator.
     * @param bool $isStartToken true for the start token.
     *
     * @return static
     */
    protected function advanceToNextToken(Iterator $tokens, bool $isStartToken)
    {
        if (($isStartToken) || ($tokens->valid() && !$this->isEndToken($tokens->current()))) {
            $tokens->next();
        }
        return $this;
    }

    /**
     * Iterates over the passed token Iterator and returns an array of parsed
     * IHeaderPart objects.
     *
     * The method checks each token to see if the token matches a sub-consumer's
     * start token, or if it matches the current consumer's end token to stop
     * processing.
     *
     * If a sub-consumer's start token is matched, the sub-consumer is invoked
     * and its returned parts are merged to the current consumer's header parts.
     *
     * After all tokens are read and an array of Header\Parts are constructed,
     * the array is passed to AbstractConsumer::processParts for any final
     * processing.
     *
     * @param Iterator $tokens An iterator over a string of tokens
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[] An array of
     *         parsed parts
     */
    protected function parseTokensIntoParts(Iterator $tokens) : array
    {
        $parts = [];
        while ($tokens->valid() && !$this->isEndToken($tokens->current())) {
            $parts = \array_merge($parts, $this->getTokenParts($tokens));
            $this->advanceToNextToken($tokens, false);
        }
        return $this->processParts($parts);
    }

    /**
     * Performs any final processing on the array of parsed parts before
     * returning it to the consumer client.
     *
     * The default implementation simply returns the passed array after
     * filtering out null/empty parts.
     *
     * @param \ZBateson\MailMimeParser\Header\IHeaderPart[] $parts The parsed
     *        parts.
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[] Array of resulting
     *         final parts.
     */
    protected function processParts(array $parts) : array
    {
        return \array_values(\array_filter($parts));
    }
}
