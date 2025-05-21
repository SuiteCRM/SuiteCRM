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

namespace Google\Service\AlertCenter;

class ResourceInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $chatAttachmentId;
  /**
   * @var string
   */
  public $chatMessageId;
  /**
   * @var string
   */
  public $deviceId;
  /**
   * @var string
   */
  public $documentId;
  /**
   * @var string
   */
  public $resourceTitle;

  /**
   * @param string
   */
  public function setChatAttachmentId($chatAttachmentId)
  {
    $this->chatAttachmentId = $chatAttachmentId;
  }
  /**
   * @return string
   */
  public function getChatAttachmentId()
  {
    return $this->chatAttachmentId;
  }
  /**
   * @param string
   */
  public function setChatMessageId($chatMessageId)
  {
    $this->chatMessageId = $chatMessageId;
  }
  /**
   * @return string
   */
  public function getChatMessageId()
  {
    return $this->chatMessageId;
  }
  /**
   * @param string
   */
  public function setDeviceId($deviceId)
  {
    $this->deviceId = $deviceId;
  }
  /**
   * @return string
   */
  public function getDeviceId()
  {
    return $this->deviceId;
  }
  /**
   * @param string
   */
  public function setDocumentId($documentId)
  {
    $this->documentId = $documentId;
  }
  /**
   * @return string
   */
  public function getDocumentId()
  {
    return $this->documentId;
  }
  /**
   * @param string
   */
  public function setResourceTitle($resourceTitle)
  {
    $this->resourceTitle = $resourceTitle;
  }
  /**
   * @return string
   */
  public function getResourceTitle()
  {
    return $this->resourceTitle;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ResourceInfo::class, 'Google_Service_AlertCenter_ResourceInfo');
