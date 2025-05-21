<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\NetworkServices;

class HttpRouteHttpDirectResponse extends \Google\Model
{
  /**
   * @var string
   */
  public $bytesBody;
  /**
   * @var int
   */
  public $status;
  /**
   * @var string
   */
  public $stringBody;

  /**
   * @param string
   */
  public function setBytesBody($bytesBody)
  {
    $this->bytesBody = $bytesBody;
  }
  /**
   * @return string
   */
  public function getBytesBody()
  {
    return $this->bytesBody;
  }
  /**
   * @param int
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return int
   */
  public function getStatus()
  {
    return $this->status;
  }
  /**
   * @param string
   */
  public function setStringBody($stringBody)
  {
    $this->stringBody = $stringBody;
  }
  /**
   * @return string
   */
  public function getStringBody()
  {
    return $this->stringBody;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HttpRouteHttpDirectResponse::class, 'Google_Service_NetworkServices_HttpRouteHttpDirectResponse');
