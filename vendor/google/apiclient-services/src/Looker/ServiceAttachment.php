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

namespace Google\Service\Looker;

class ServiceAttachment extends \Google\Model
{
  /**
   * @var string
   */
  public $connectionStatus;
  /**
   * @var string
   */
  public $localFqdn;
  /**
   * @var string
   */
  public $targetServiceAttachmentUri;

  /**
   * @param string
   */
  public function setConnectionStatus($connectionStatus)
  {
    $this->connectionStatus = $connectionStatus;
  }
  /**
   * @return string
   */
  public function getConnectionStatus()
  {
    return $this->connectionStatus;
  }
  /**
   * @param string
   */
  public function setLocalFqdn($localFqdn)
  {
    $this->localFqdn = $localFqdn;
  }
  /**
   * @return string
   */
  public function getLocalFqdn()
  {
    return $this->localFqdn;
  }
  /**
   * @param string
   */
  public function setTargetServiceAttachmentUri($targetServiceAttachmentUri)
  {
    $this->targetServiceAttachmentUri = $targetServiceAttachmentUri;
  }
  /**
   * @return string
   */
  public function getTargetServiceAttachmentUri()
  {
    return $this->targetServiceAttachmentUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ServiceAttachment::class, 'Google_Service_Looker_ServiceAttachment');
