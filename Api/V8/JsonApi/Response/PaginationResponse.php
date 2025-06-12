<?php
namespace Api\V8\JsonApi\Response;

class PaginationResponse extends LinksResponse
{
    /**
     * @var string
     */
    private $first;

    /**
     * @var string
     */
    private $prev;

    /**
     * @var string
     */
    private $next;

    /**
     * @var string
     */
    private $last;

    /**
     * @return string
     */
    public function getFirst()
    {
        return $this->first;
    }

    /**
     * @param string $first
     */
    public function setFirst($first)
    {
        $this->first = $first;
    }

    /**
     * @return string
     */
    public function getPrev()
    {
        return $this->prev;
    }

    /**
     * @param string $prev
     */
    public function setPrev($prev)
    {
        $this->prev = $prev;
    }

    /**
     * @return string
     */
    public function getNext()
    {
        return $this->next;
    }

    /**
     * @param string $next
     */
    public function setNext($next)
    {
        $this->next = $next;
    }

    /**
     * @return string
     */
    public function getLast()
    {
        return $this->last;
    }

    /**
     * @param string $last
     */
    public function setLast($last)
    {
        $this->last = $last;
    }

    /**
     * @inheritdoc
     */
    public function jsonSerialize(): mixed
    {
        return [
            'first' => $this->getFirst(),
            'prev' => $this->getPrev(),
            'next' => $this->getNext(),
            'last' => $this->getLast()
        ];
    }
}
