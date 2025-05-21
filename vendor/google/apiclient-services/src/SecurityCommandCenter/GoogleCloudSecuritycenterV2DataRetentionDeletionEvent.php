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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2DataRetentionDeletionEvent extends \Google\Model
{
  /**
   * @var string
   */
  public $dataObjectCount;
  /**
   * @var string
   */
  public $eventDetectionTime;
  /**
   * @var string
   */
  public $eventType;
  /**
   * @var string
   */
  public $maxRetentionAllowed;

  /**
   * @param string
   */
  public function setDataObjectCount($dataObjectCount)
  {
    $this->dataObjectCount = $dataObjectCount;
  }
  /**
   * @return string
   */
  public function getDataObjectCount()
  {
    return $this->dataObjectCount;
  }
  /**
   * @param string
   */
  public function setEventDetectionTime($eventDetectionTime)
  {
    $this->eventDetectionTime = $eventDetectionTime;
  }
  /**
   * @return string
   */
  public function getEventDetectionTime()
  {
    return $this->eventDetectionTime;
  }
  /**
   * @param string
   */
  public function setEventType($eventType)
  {
    $this->eventType = $eventType;
  }
  /**
   * @return string
   */
  public function getEventType()
  {
    return $this->eventType;
  }
  /**
   * @param string
   */
  public function setMaxRetentionAllowed($maxRetentionAllowed)
  {
    $this->maxRetentionAllowed = $maxRetentionAllowed;
  }
  /**
   * @return string
   */
  public function getMaxRetentionAllowed()
  {
    return $this->maxRetentionAllowed;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2DataRetentionDeletionEvent::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2DataRetentionDeletionEvent');
