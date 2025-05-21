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

class GoogleCloudSecuritycenterV2NotificationMessage extends \Google\Model
{
  protected $findingType = GoogleCloudSecuritycenterV2Finding::class;
  protected $findingDataType = '';
  /**
   * @var string
   */
  public $notificationConfigName;
  protected $resourceType = GoogleCloudSecuritycenterV2Resource::class;
  protected $resourceDataType = '';

  /**
   * @param GoogleCloudSecuritycenterV2Finding
   */
  public function setFinding(GoogleCloudSecuritycenterV2Finding $finding)
  {
    $this->finding = $finding;
  }
  /**
   * @return GoogleCloudSecuritycenterV2Finding
   */
  public function getFinding()
  {
    return $this->finding;
  }
  /**
   * @param string
   */
  public function setNotificationConfigName($notificationConfigName)
  {
    $this->notificationConfigName = $notificationConfigName;
  }
  /**
   * @return string
   */
  public function getNotificationConfigName()
  {
    return $this->notificationConfigName;
  }
  /**
   * @param GoogleCloudSecuritycenterV2Resource
   */
  public function setResource(GoogleCloudSecuritycenterV2Resource $resource)
  {
    $this->resource = $resource;
  }
  /**
   * @return GoogleCloudSecuritycenterV2Resource
   */
  public function getResource()
  {
    return $this->resource;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2NotificationMessage::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2NotificationMessage');
