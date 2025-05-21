<?php
namespace GuzzleHttp\Ring\Future;

use React\Promise\PromiseInterface;

/**
 * Represents the result of a computation that may not have completed yet.
 *
 * You can use the future in a blocking manner using the wait() function, or
 * you can use a promise from the future to receive the result when the future
 * has been resolved.
 *
 * When the future is dereferenced using wait(), the result of the computation
 * is cached and returned for subsequent calls to wait(). If the result of the
 * computation has not yet completed when wait() is called, the call to wait()
 * will block until the future has completed.
 */
interface FutureInterface
{
    /**
     * Returns the result of the future either from cache or by blocking until
     * it is complete.
     *
     * This method must block until the future has a result or is cancelled.
     * Throwing an exception in the wait() method will mark the future as
     * realized and will throw the exception each time wait() is called.
     * Throwing an instance of GuzzleHttp\Ring\CancelledException will mark
     * the future as realized, will not throw immediately, but will throw the
     * exception if the future's wait() method is called again.
     *
     * @return mixed
     */
    public function wait();

    /**
     * Cancels the future, if possible.
     */
    public function cancel();

    /**
     * Transforms a promise's value by applying a function to the promise's fulfillment
     * or rejection value. Returns a new promise for the transformed result.
     *
     * The `then()` method registers new fulfilled and rejection handlers with a promise
     * (all parameters are optional):
     *
     *  * `$onFulfilled` will be invoked once the promise is fulfilled and passed
     *     the result as the first argument.
     *  * `$onRejected` will be invoked once the promise is rejected and passed the
     *     reason as the first argument.
     *  * `$onProgress` (deprecated) will be invoked whenever the producer of the promise
     *     triggers progress notifications and passed a single argument (whatever it
     *     wants) to indicate progress.
     *
     * It returns a new promise that will fulfill with the return value of either
     * `$onFulfilled` or `$onRejected`, whichever is called, or will reject with
     * the thrown exception if either throws.
     *
     * A promise makes the following guarantees about handlers registered in
     * the same call to `then()`:
     *
     *  1. Only one of `$onFulfilled` or `$onRejected` will be called,
     *      never both.
     *  2. `$onFulfilled` and `$onRejected` will never be called more
     *      than once.
     *  3. `$onProgress` (deprecated) may be called multiple times.
     *
     * @param callable|null $onFulfilled
     * @param callable|null $onRejected
     * @return PromiseInterface
     */
    public function then(?callable $onFulfilled = null, ?callable $onRejected = null);

    /**
     * Returns the promise of the deferred.
     *
     * @return PromiseInterface
     */
    public function promise();
}
