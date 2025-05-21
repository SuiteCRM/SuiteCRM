<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

use AppendIterator;
use ArrayIterator;
use Iterator;
use RecursiveIteratorIterator;
use ZBateson\MailMimeParser\MailMimeParser;

/**
 * A message part that contains children.
 *
 * @author Zaahid Bateson
 */
abstract class MultiPart extends MessagePart implements IMultiPart
{
    /**
     * @var PartChildrenContainer child part container
     */
    protected $partChildrenContainer;

    public function __construct(
        ?IMimePart $parent = null,
        ?PartStreamContainer $streamContainer = null,
        ?PartChildrenContainer $partChildrenContainer = null
    ) {
        parent::__construct($streamContainer, $parent);
        if ($partChildrenContainer === null) {
            $di = MailMimeParser::getDependencyContainer();
            $partChildrenContainer = $di[\ZBateson\MailMimeParser\Message\PartChildrenContainer::class];
        }
        $this->partChildrenContainer = $partChildrenContainer;
    }

    private function getAllPartsIterator() : AppendIterator
    {
        $iter = new AppendIterator();
        $iter->append(new ArrayIterator([$this]));
        $iter->append(new RecursiveIteratorIterator($this->partChildrenContainer, RecursiveIteratorIterator::SELF_FIRST));
        return $iter;
    }

    private function iteratorFindAt(Iterator $iter, $index, $fnFilter = null)
    {
        $pos = 0;
        foreach ($iter as $part) {
            if (($fnFilter === null || $fnFilter($part))) {
                if ($index === $pos) {
                    return $part;
                }
                ++$pos;
            }
        }
    }

    public function getPart($index, $fnFilter = null)
    {
        return $this->iteratorFindAt(
            $this->getAllPartsIterator(),
            $index,
            $fnFilter
        );
    }

    public function getAllParts($fnFilter = null)
    {
        $array = \iterator_to_array($this->getAllPartsIterator(), false);
        if ($fnFilter !== null) {
            return \array_values(\array_filter($array, $fnFilter));
        }
        return $array;
    }

    public function getPartCount($fnFilter = null)
    {
        return \count($this->getAllParts($fnFilter));
    }

    public function getChild($index, $fnFilter = null)
    {
        return $this->iteratorFindAt(
            $this->partChildrenContainer,
            $index,
            $fnFilter
        );
    }

    public function getChildIterator()
    {
        return $this->partChildrenContainer;
    }

    public function getChildParts($fnFilter = null)
    {
        $array = \iterator_to_array($this->partChildrenContainer, false);
        if ($fnFilter !== null) {
            return \array_values(\array_filter($array, $fnFilter));
        }
        return $array;
    }

    public function getChildCount($fnFilter = null)
    {
        return \count($this->getChildParts($fnFilter));
    }

    public function getPartByMimeType($mimeType, $index = 0)
    {
        return $this->getPart($index, PartFilter::fromContentType($mimeType));
    }

    public function getAllPartsByMimeType($mimeType)
    {
        return $this->getAllParts(PartFilter::fromContentType($mimeType));
    }

    public function getCountOfPartsByMimeType($mimeType)
    {
        return $this->getPartCount(PartFilter::fromContentType($mimeType));
    }

    public function getPartByContentId($contentId)
    {
        $sanitized = \preg_replace('/^\s*<|>\s*$/', '', $contentId);
        return $this->getPart(0, function(IMessagePart $part) use ($sanitized) {
            $cid = $part->getContentId();
            return ($cid !== null && \strcasecmp($cid, $sanitized) === 0);
        });
    }

    public function addChild(IMessagePart $part, ?int $position = null)
    {
        if ($part !== $this) {
            $part->parent = $this;
            $this->partChildrenContainer->add($part, $position);
            $this->notify();
        }
    }

    public function removePart(IMessagePart $part) : ?int
    {
        $parent = $part->getParent();
        if ($this !== $parent && $parent !== null) {
            return $parent->removePart($part);
        }

        $position = $this->partChildrenContainer->remove($part);
        if ($position !== null) {
            $this->notify();
        }
        return $position;
    }

    public function removeAllParts($fnFilter = null) : int
    {
        $parts = $this->getAllParts($fnFilter);
        $count = \count($parts);
        foreach ($parts as $part) {
            if ($part === $this) {
                --$count;
                continue;
            }
            $this->removePart($part);
        }
        return $count;
    }
}
