<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

/**
 * An interface representing a non-mime uuencoded part.
 *
 * Prior to the MIME standard, a plain text email may have included attachments
 * below it surrounded by 'begin' and 'end' lines and uuencoded data between
 * them.  Those attachments are captured as 'IUUEncodedPart' objects.
 *
 * The 'begin' line included a file name and unix file mode.  IUUEncodedPart
 * allows reading/setting those parameters.
 *
 * IUUEncodedPart returns a Content-Transfer-Encoding of x-uuencode, a
 * Content-Type of application-octet-stream, and a Content-Disposition of
 * 'attachment'.  It also expects a mode and filename to initialize it, and
 * adds 'filename' parts to the Content-Disposition and a 'name' parameter to
 * Content-Type.
 *
 * @author Zaahid Bateson
 */
interface IUUEncodedPart extends IMessagePart
{
    /**
     * Sets the filename included in the uuencoded 'begin' line.
     *
     */
    public function setFilename(string $filename);

    /**
     * Returns the file mode included in the uuencoded 'begin' line for this
     * part.
     */
    public function getUnixFileMode() : ?int;

    /**
     * Sets the unix file mode for the uuencoded 'begin' line.
     */
    public function setUnixFileMode(int $mode);
}
