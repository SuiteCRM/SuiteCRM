<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

use ArrayIterator;
use IteratorAggregate;
use ZBateson\MailMimeParser\Header\HeaderFactory;
use ZBateson\MailMimeParser\Header\IHeader;

/**
 * Maintains a collection of headers for a part.
 *
 * @author Zaahid Bateson
 */
class PartHeaderContainer implements IteratorAggregate
{
    /**
     * @var HeaderFactory the HeaderFactory object used for created headers
     */
    protected $headerFactory;

    /**
     * @var string[][] Each element in the array is an array with its first
     * element set to the header's name, and the second its value.
     */
    private $headers = [];

    /**
     * @var \ZBateson\MailMimeParser\Header\IHeader[] Each element is an IHeader
     *      representing the header at the same index in the $headers array.  If
     *      an IHeader has not been constructed for the header at that index,
     *      the element would be set to null.
     */
    private $headerObjects = [];

    /**
     * @var array Maps header names by their "normalized" (lower-cased,
     *      non-alphanumeric characters stripped) name to an array of indexes in
     *      the $headers array.  For example:
     *      $headerMap['contenttype'] = [ 1, 4 ]
     *      would indicate that the headers in $headers[1] and $headers[4] are
     *      both headers with the name 'Content-Type' or 'contENTtype'.
     */
    private $headerMap = [];

    /**
     * @var int the next index to use for $headers and $headerObjects.
     */
    private $nextIndex = 0;

    /**
     * Pass a PartHeaderContainer as the second parameter.  This is useful when
     * creating a new MimePart with this PartHeaderContainer and the original
     * container is needed for parsing and changes to the header in the part
     * should not affect parsing.
     *
     * @param PartHeaderContainer $cloneSource the original container to clone
     *        from
     */
    public function __construct(HeaderFactory $headerFactory, ?PartHeaderContainer $cloneSource = null)
    {
        $this->headerFactory = $headerFactory;
        if ($cloneSource !== null) {
            $this->headers = $cloneSource->headers;
            $this->headerObjects = $cloneSource->headerObjects;
            $this->headerMap = $cloneSource->headerMap;
            $this->nextIndex = $cloneSource->nextIndex;
        }
    }

    /**
     * Returns true if the passed header exists in this collection.
     *
     * @param string $name
     * @param int $offset
     * @return bool
     */
    public function exists($name, $offset = 0)
    {
        $s = $this->headerFactory->getNormalizedHeaderName($name);
        return isset($this->headerMap[$s][$offset]);
    }

    /**
     * Returns an array of header indexes with names that more closely match
     * the passed $name if available: for instance if there are two headers in
     * an email, "Content-Type" and "ContentType", and the query is for a header
     * with the name "Content-Type", only headers that match exactly
     * "Content-Type" would be returned.
     *
     * @return int[]|null
     */
    private function getAllWithOriginalHeaderNameIfSet(string $name) : ?array
    {
        $s = $this->headerFactory->getNormalizedHeaderName($name);
        if (isset($this->headerMap[$s])) {
            $self = $this;
            $filtered = \array_filter($this->headerMap[$s], function($h) use ($name, $self) {
                return (\strcasecmp($self->headers[$h][0], $name) === 0);
            });
            return (!empty($filtered)) ? $filtered : $this->headerMap[$s];
        }
        return null;
    }

    /**
     * Returns the IHeader object for the header with the given $name, or null
     * if none exist.
     *
     * An optional offset can be provided, which defaults to the first header in
     * the collection when more than one header with the same name exists.
     *
     * Note that mime headers aren't case sensitive.
     *
     * @param string $name
     * @param int $offset
     * @return \ZBateson\MailMimeParser\Header\IHeader|null
     */
    public function get(string $name, int $offset = 0)
    {
        $a = $this->getAllWithOriginalHeaderNameIfSet($name);
        if (!empty($a) && isset($a[$offset])) {
            return $this->getByIndex($a[$offset]);
        }
        return null;
    }

    /**
     * Returns the IHeader object for the header with the given $name, or null
     * if none exist, using the passed $iHeaderClass to construct it.
     *
     * An optional offset can be provided, which defaults to the first header in
     * the collection when more than one header with the same name exists.
     *
     * Note that mime headers aren't case sensitive.
     *
     * @param string $name
     * @param string $iHeaderClass
     * @param int $offset
     * @return ?IHeader
     */
    public function getAs(string $name, string $iHeaderClass, int $offset = 0) : ?IHeader
    {
        $a = $this->getAllWithOriginalHeaderNameIfSet($name);
        if (!empty($a) && isset($a[$offset])) {
            return $this->getByIndexAs($a[$offset], $iHeaderClass);
        }
        return null;
    }

    /**
     * Returns all headers with the passed name.
     *
     * @param string $name
     * @return \ZBateson\MailMimeParser\Header\IHeader[]
     */
    public function getAll($name)
    {
        $a = $this->getAllWithOriginalHeaderNameIfSet($name);
        if (!empty($a)) {
            $self = $this;
            return \array_map(function($index) use ($self) {
                return $self->getByIndex($index);
            }, $a);
        }
        return [];
    }

    /**
     * Returns the header in the headers array at the passed 0-based integer
     * index or null if one doesn't exist.
     *
     * @return \ZBateson\MailMimeParser\Header\IHeader|null
     */
    private function getByIndex(int $index)
    {
        if (!isset($this->headers[$index])) {
            return null;
        }
        if ($this->headerObjects[$index] === null) {
            $this->headerObjects[$index] = $this->headerFactory->newInstance(
                $this->headers[$index][0],
                $this->headers[$index][1]
            );
        }
        return $this->headerObjects[$index];
    }

    /**
     * Returns the header in the headers array at the passed 0-based integer
     * index or null if one doesn't exist, using the passed $iHeaderClass to
     * construct it.
     *
     * @return \ZBateson\MailMimeParser\Header\IHeader|null
     */
    private function getByIndexAs(int $index, string $iHeaderClass) : ?IHeader
    {
        if (!isset($this->headers[$index])) {
            return null;
        }
        if ($this->headerObjects[$index] !== null && \get_class($this->headerObjects[$index]) === $iHeaderClass) {
            return $this->headerObjects[$index];
        }
        return $this->headerFactory->newInstanceOf(
            $this->headers[$index][0],
            $this->headers[$index][1],
            $iHeaderClass
        );
    }

    /**
     * Removes the header from the collection with the passed name.  Defaults to
     * removing the first instance of the header for a collection that contains
     * more than one with the same passed name.
     *
     * @param string $name
     * @param int $offset
     * @return bool if a header was removed.
     */
    public function remove($name, $offset = 0)
    {
        $s = $this->headerFactory->getNormalizedHeaderName($name);
        if (isset($this->headerMap[$s][$offset])) {
            $index = $this->headerMap[$s][$offset];
            \array_splice($this->headerMap[$s], $offset, 1);
            unset($this->headers[$index], $this->headerObjects[$index]);

            return true;
        }
        return false;
    }

    /**
     * Removes all headers that match the passed name.
     *
     * @param string $name
     * @return bool true if one or more headers were removed.
     */
    public function removeAll($name)
    {
        $s = $this->headerFactory->getNormalizedHeaderName($name);
        if (!empty($this->headerMap[$s])) {
            foreach ($this->headerMap[$s] as $i) {
                unset($this->headers[$i], $this->headerObjects[$i]);

            }
            $this->headerMap[$s] = [];
            return true;
        }
        return false;
    }

    /**
     * Adds the header to the collection.
     *
     * @param string $name
     * @param string $value
     */
    public function add($name, $value)
    {
        $s = $this->headerFactory->getNormalizedHeaderName($name);
        $this->headers[$this->nextIndex] = [$name, $value];
        $this->headerObjects[$this->nextIndex] = null;
        if (!isset($this->headerMap[$s])) {
            $this->headerMap[$s] = [];
        }
        $this->headerMap[$s][] = $this->nextIndex;
        $this->nextIndex++;
    }

    /**
     * If a header exists with the passed name, and at the passed offset if more
     * than one exists, its value is updated.
     *
     * If a header with the passed name doesn't exist at the passed offset, it
     * is created at the next available offset (offset is ignored when adding).
     *
     * @param string $name
     * @param string $value
     * @param int $offset
     */
    public function set($name, $value, $offset = 0) : self
    {
        $s = $this->headerFactory->getNormalizedHeaderName($name);
        if (!isset($this->headerMap[$s][$offset])) {
            $this->add($name, $value);
            return $this;
        }
        $i = $this->headerMap[$s][$offset];
        $this->headers[$i] = [$name, $value];
        $this->headerObjects[$i] = null;
        return $this;
    }

    /**
     * Returns an array of IHeader objects representing all headers in this
     * collection.
     *
     * @return \ZBateson\MailMimeParser\Header\IHeader[]
     */
    public function getHeaderObjects()
    {
        return \array_filter(\array_map([$this, 'getByIndex'], \array_keys($this->headers)));
    }

    /**
     * Returns an array of headers in this collection.  Each returned element in
     * the array is an array with the first element set to the name, and the
     * second its value:
     *
     * [
     *     [ 'Header-Name', 'Header Value' ],
     *     [ 'Second-Header-Name', 'Second-Header-Value' ],
     *     // etc...
     * ]
     *
     * @return string[][]
     */
    public function getHeaders()
    {
        return \array_values(\array_filter($this->headers));
    }

    /**
     * Returns an iterator to the headers in this collection.  Each returned
     * element is an array with its first element set to the header's name, and
     * the second to its value:
     *
     * [ 'Header-Name', 'Header Value' ]
     *
     * @return ArrayIterator
     */
    #[\ReturnTypeWillChange]
    public function getIterator()
    {
        return new ArrayIterator($this->getHeaders());
    }
}
