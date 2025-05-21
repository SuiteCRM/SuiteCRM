<?php
namespace GuzzleHttp\Ring\Future;

use React\Promise\PromiseInterface;

use function React\Promise\reject;
use function React\Promise\resolve;

/**
 * Represents a future value that has been resolved or rejected.
 */
class CompletedFutureValue implements FutureInterface
{
    protected $result;
    protected $error;

    private $cachedPromise;

    /**
     * @param mixed      $result Resolved result
     * @param \Exception $e      Error. Pass a GuzzleHttp\Ring\Exception\CancelledFutureAccessException
     *                           to mark the future as cancelled.
     */
    public function __construct($result, ?\Exception $e = null)
    {
        $this->result = $result;
        $this->error = $e;
    }

    /**
     * @return mixed
     */
    public function wait()
    {
        if ($this->error) {
            throw $this->error;
        }

        return $this->result;
    }

    public function cancel(): void
    {}

    /**
     * @return PromiseInterface
     */
    public function promise()
    {
        if (!$this->cachedPromise) {
            $this->cachedPromise = $this->error
                ? reject($this->error)
                : resolve($this->result);
        }

        return $this->cachedPromise;
    }

    /**
     * @return PromiseInterface
     */
    public function then(
        ?callable $onFulfilled = null,
        ?callable $onRejected = null
    ) {
        return $this->promise()->then($onFulfilled, $onRejected);
    }
}
