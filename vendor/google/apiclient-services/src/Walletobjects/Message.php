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

namespace Google\Service\Walletobjects;

class Message extends \Google\Model
{
  /**
   * @var string
   */
  public $body;
  protected $displayIntervalType = TimeInterval::class;
  protected $displayIntervalDataType = '';
  /**
   * @var string
   */
  public $header;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $kind;
  protected $localizedBodyType = LocalizedString::class;
  protected $localizedBodyDataType = '';
  protected $localizedHeaderType = LocalizedString::class;
  protected $localizedHeaderDataType = '';
  /**
   * @var string
   */
  public $messageType;

  /**
   * @param string
   */
  public function setBody($body)
  {
    $this->body = $body;
  }
  /**
   * @return string
   */
  public function getBody()
  {
    return $this->body;
  }
  /**
   * @param TimeInterval
   */
  public function setDisplayInterval(TimeInterval $displayInterval)
  {
    $this->displayInterval = $displayInterval;
  }
  /**
   * @return TimeInterval
   */
  public function getDisplayInterval()
  {
    return $this->displayInterval;
  }
  /**
   * @param string
   */
  public function setHeader($header)
  {
    $this->header = $header;
  }
  /**
   * @return string
   */
  public function getHeader()
  {
    return $this->header;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedBody(LocalizedString $localizedBody)
  {
    $this->localizedBody = $localizedBody;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedBody()
  {
    return $this->localizedBody;
  }
  /**
   * @param LocalizedString
   */
  public function setLocalizedHeader(LocalizedString $localizedHeader)
  {
    $this->localizedHeader = $localizedHeader;
  }
  /**
   * @return LocalizedString
   */
  public function getLocalizedHeader()
  {
    return $this->localizedHeader;
  }
  /**
   * @param string
   */
  public function setMessageType($messageType)
  {
    $this->messageType = $messageType;
  }
  /**
   * @return string
   */
  public function getMessageType()
  {
    return $this->messageType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Message::class, 'Google_Service_Walletobjects_Message');
