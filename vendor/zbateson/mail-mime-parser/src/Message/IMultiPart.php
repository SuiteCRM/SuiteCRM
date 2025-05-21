<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

/**
 * An interface representing a message part that contains children.
 *
 * An IMultiPart object may have any number of child parts, or may be a child
 * itself with its own parent or parents.
 *
 * @author Zaahid Bateson
 */
interface IMultiPart extends IMessagePart
{
    /**
     * Returns the part at the given 0-based index for this part (part 0) and
     * all parts under it, or null if not found with the passed filter function.
     *
     * Note that the first part returned is the current part itself.  This is
     * usually desirable for queries with a passed filter, e.g. looking for an
     * part with a specific Content-Type that may be satisfied by the current
     * part.
     *
     * The passed callable must accept an {@see IMessagePart} as an argument,
     * and return true if it should be accepted, or false to filter the part
     * out.  Some default filters are provided by static functions returning
     * callables in {@see PartFilter}.
     *
     * @see IMultiPart::getAllParts() to get an array of all parts with an
     *      optional filter.
     * @see IMultiPart::getPartCount() to get the number of parts with an
     *      optional filter.
     * @see IMultiPart::getChild() to get a direct child of the current part.
     * @param int $index The 0-based index (0 being this part if $fnFilter is
     *        null or this part is satisfied by the filter).
     * @param callable $fnFilter Optional function accepting an IMessagePart and
     *        returning true if the part should be included.
     * @return IMessagePart|null A matching part, or null if not found.
     */
    public function getPart($index, $fnFilter = null);

    /**
     * Returns the current part, all child parts, and child parts of all
     * children optionally filtering them with the provided PartFilter.
     *
     * Note that the first part returned is the current part itself.  This is
     * often desirable for queries with a passed filter, e.g. looking for an
     * IMessagePart with a specific Content-Type that may be satisfied by the
     * current part.
     *
     * The passed callable must accept an {@see IMessagePart} as an argument,
     * and return true if it should be accepted, or false to filter the part
     * out.  Some default filters are provided by static functions returning
     * callables in {@see PartFilter}.
     *
     * @see IMultiPart::getPart() to find a part at a specific 0-based index
     *      with an optional filter.
     * @see IMultiPart::getPartCount() to get the number of parts with an
     *      optional filter.
     * @see IMultiPart::getChildParts() to get an array of all direct children
     *      of the current part.
     * @param callable $fnFilter Optional function accepting an IMessagePart and
     *        returning true if the part should be included.
     * @return IMessagePart[] An array of matching parts.
     */
    public function getAllParts($fnFilter = null);

    /**
     * Returns the total number of parts in this and all children.
     *
     * Note that the current part is considered, so the minimum getPartCount is
     * 1 without a filter.
     *
     * The passed callable must accept an {@see IMessagePart} as an argument,
     * and return true if it should be accepted, or false to filter the part
     * out.  Some default filters are provided by static functions returning
     * callables in {@see PartFilter}.
     *
     * @see IMultiPart::getPart() to find a part at a specific 0-based index
     *      with an optional filter.
     * @see IMultiPart::getAllParts() to get an array of all parts with an
     *      optional filter.
     * @see IMultiPart::getChildCount() to get a count of direct children of
     *      this part.
     * @param callable $fnFilter Optional function accepting an IMessagePart and
     *        returning true if the part should be included.
     * @return int The number of matching parts.
     */
    public function getPartCount($fnFilter = null);

    /**
     * Returns the direct child at the given 0-based index and optional filter,
     * or null if none exist or do not match.
     *
     * The passed callable must accept an {@see IMessagePart} as an argument,
     * and return true if it should be accepted, or false to filter the part
     * out.  Some default filters are provided by static functions returning
     * callables in {@see PartFilter}.
     *
     * @see IMultiPart::getChildParts() to get an array of all direct children
     *      of the current part.
     * @see IMultiPart::getChildCount() to get a count of direct children of
     *      this part.
     * @see IMultiPart::getChildIterator() to get an iterator of children of
     *      this part.
     * @see IMultiPart::getPart() to find a part at a specific 0-based index
     *      with an optional filter.
     * @param int $index 0-based index
     * @param callable $fnFilter Optional function accepting an IMessagePart and
     *        returning true if the part should be included.
     * @return IMessagePart|null The matching direct child part or null if not
     *         found.
     */
    public function getChild($index, $fnFilter = null);

    /**
     * Returns an array of all direct child parts, optionally filtering them
     * with a passed callable.
     *
     * The passed callable must accept an {@see IMessagePart} as an argument,
     * and return true if it should be accepted, or false to filter the part
     * out.  Some default filters are provided by static functions returning
     * callables in {@see PartFilter}.
     *
     * @see IMultiPart::getChild() to get a direct child of the current part.
     * @see IMultiPart::getChildCount() to get a count of direct children of
     *      this part.
     * @see IMultiPart::getChildIterator() to get an iterator of children of
     *      this part.
     * @see IMultiPart::getAllParts() to get an array of all parts with an
     *      optional filter.
     * @param callable $fnFilter Optional function accepting an IMessagePart and
     *        returning true if the part should be included.
     * @return IMessagePart[] An array of matching child parts.
     */
    public function getChildParts($fnFilter = null);

    /**
     * Returns the number of direct children under this part (optionally
     * counting only filtered items if a callable filter is passed).
     *
     * The passed callable must accept an {@see IMessagePart} as an argument,
     * and return true if it should be accepted, or false to filter the part
     * out.  Some default filters are provided by static functions returning
     * callables in {@see PartFilter}.
     *
     * @see IMultiPart::getChild() to get a direct child of the current part.
     * @see IMultiPart::getChildParts() to get an array of all direct children
     *      of the current part.
     * @see IMultiPart::getChildIterator() to get an iterator of children of
     *      this part.
     * @see IMultiPart::getPartCount() to get the number of parts with an
     *      optional filter.
     * @param callable $fnFilter Optional function accepting an IMessagePart and
     *        returning true if the part should be included.
     * @return int The number of children, or number of children matching the
     *         the passed filtering callable.
     */
    public function getChildCount($fnFilter = null);

    /**
     * Returns a \RecursiveIterator of child parts.
     *
     * The {@see https://www.php.net/manual/en/class.recursiveiterator.php \RecursiveIterator}
     * allows iterating over direct children, or using
     * a {@see https://www.php.net/manual/en/class.recursiveiteratoriterator.php \RecursiveIteratorIterator}
     * to iterate over direct children, and all their children.
     *
     * @see https://www.php.net/manual/en/class.recursiveiterator.php
     *      RecursiveIterator
     * @see https://www.php.net/manual/en/class.recursiveiteratoriterator.php
     *      RecursiveIteratorIterator
     * @see IMultiPart::getChild() to get a direct child of the current part.
     * @see IMultiPart::getChildParts() to get an array of all direct children
     *      of the current part.
     * @see IMultiPart::getChildCount() to get a count of direct children of
     *      this part.
     * @see IMultiPart::getAllParts() to get an array of all parts with an
     *      optional filter.
     * @return \RecursiveIterator
     */
    public function getChildIterator();

    /**
     * Returns the part that has a content type matching the passed mime type at
     * the given index, or null if there are no matching parts.
     *
     * Creates a filter that looks at the return value of
     * {@see IMessagePart::getContentType()} for all parts (including the
     * current part) and returns a matching one at the given 0-based index.
     *
     * @see IMultiPart::getAllPartsByMimeType() to get all parts that match a
     *      mime type.
     * @see IMultiPart::getCountOfPartsByMimeType() to get a count of parts with
     *      a mime type.
     * @param string $mimeType The mime type to find.
     * @param int $index Optional 0-based index (defaulting to '0').
     * @return IMessagePart|null The part.
     */
    public function getPartByMimeType($mimeType, $index = 0);

    /**
     * Returns an array of all parts that have a content type matching the
     * passed mime type.
     *
     * Creates a filter that looks at the return value of
     * {@see IMessagePart::getContentType()} for all parts (including the
     * current part), returning an array of matching parts.
     *
     * @see IMultiPart::getPartByMimeType() to get a part by mime type.
     * @see IMultiPart::getCountOfPartsByMimeType() to get a count of parts with
     *      a mime type.
     * @param string $mimeType The mime type to find.
     * @return IMessagePart[] An array of matching parts.
     */
    public function getAllPartsByMimeType($mimeType);

    /**
     * Returns the number of parts that have content types matching the passed
     * mime type.
     *
     * @see IMultiPart::getPartByMimeType() to get a part by mime type.
     * @see IMultiPart::getAllPartsByMimeType() to get all parts that match a
     *      mime type.
     * @param string $mimeType The mime type to find.
     * @return int The number of matching parts.
     */
    public function getCountOfPartsByMimeType($mimeType);

    /**
     * Returns a part that has the given Content ID, or null if not found.
     *
     * Calls {@see IMessagePart::getContentId()} to find a matching part.
     *
     * @param string $contentId The content ID to find a part for.
     * @return IMessagePart|null The matching part.
     */
    public function getPartByContentId($contentId);

    /**
     * Registers the passed part as a child of the current part.
     *
     * If the $position parameter is non-null, adds the part at the passed
     * position index, otherwise adds it as the last child.
     *
     * @param IMessagePart $part The part to add.
     * @param int $position Optional insertion position 0-based index.
     */
    public function addChild(IMessagePart $part, ?int $position = null);

    /**
     * Removes the child part from this part and returns its previous position
     * or null if it wasn't found.
     *
     * Note that if the part is not a direct child of this part, the returned
     * position is its index within its parent (calls removePart on its direct
     * parent).
     *
     * This also means that parts from unrelated parts/messages could be removed
     * by a call to removePart -- it will always remove the part from its parent
     * if it has one, essentially calling
     * ```php $part->getParent()->removePart(); ```.
     *
     * @param IMessagePart $part The part to remove
     * @return int|null The previous index position of the part within its old
     *         parent.
     */
    public function removePart(IMessagePart $part) : ?int;

    /**
     * Removes all parts below the current part.  If a callable filter is
     * passed, removes only those matching the passed filter.  The number of
     * removed parts is returned.
     *
     * Note: the current part will not be removed.  Although the function naming
     * matches getAllParts, which returns the current part, it also doesn't only
     * remove direct children like getChildParts.  Internally this function uses
     * getAllParts but the current part is filtered out if returned.
     *
     * @param callable $fnFilter Optional function accepting an IMessagePart and
     *        returning true if the part should be included.
     * @return int The number of removed parts.
     */
    public function removeAllParts($fnFilter = null) : int;
}
