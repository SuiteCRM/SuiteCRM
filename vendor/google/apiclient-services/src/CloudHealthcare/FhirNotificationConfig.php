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

namespace Google\Service\CloudHealthcare;

class FhirNotificationConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $pubsubTopic;
  /**
   * @var bool
   */
  public $sendFullResource;
  /**
   * @var bool
   */
  public $sendPreviousResourceOnDelete;

  /**
   * @param string
   */
  public function setPubsubTopic($pubsubTopic)
  {
    $this->pubsubTopic = $pubsubTopic;
  }
  /**
   * @return string
   */
  public function getPubsubTopic()
  {
    return $this->pubsubTopic;
  }
  /**
   * @param bool
   */
  public function setSendFullResource($sendFullResource)
  {
    $this->sendFullResource = $sendFullResource;
  }
  /**
   * @return bool
   */
  public function getSendFullResource()
  {
    return $this->sendFullResource;
  }
  /**
   * @param bool
   */
  public function setSendPreviousResourceOnDelete($sendPreviousResourceOnDelete)
  {
    $this->sendPreviousResourceOnDelete = $sendPreviousResourceOnDelete;
  }
  /**
   * @return bool
   */
  public function getSendPreviousResourceOnDelete()
  {
    return $this->sendPreviousResourceOnDelete;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FhirNotificationConfig::class, 'Google_Service_CloudHealthcare_FhirNotificationConfig');
