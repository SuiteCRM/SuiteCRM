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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1ModelMonitoringAlertConfig extends \Google\Collection
{
  protected $collection_key = 'notificationChannels';
  protected $emailAlertConfigType = GoogleCloudAiplatformV1ModelMonitoringAlertConfigEmailAlertConfig::class;
  protected $emailAlertConfigDataType = '';
  /**
   * @var bool
   */
  public $enableLogging;
  /**
   * @var string[]
   */
  public $notificationChannels;

  /**
   * @param GoogleCloudAiplatformV1ModelMonitoringAlertConfigEmailAlertConfig
   */
  public function setEmailAlertConfig(GoogleCloudAiplatformV1ModelMonitoringAlertConfigEmailAlertConfig $emailAlertConfig)
  {
    $this->emailAlertConfig = $emailAlertConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelMonitoringAlertConfigEmailAlertConfig
   */
  public function getEmailAlertConfig()
  {
    return $this->emailAlertConfig;
  }
  /**
   * @param bool
   */
  public function setEnableLogging($enableLogging)
  {
    $this->enableLogging = $enableLogging;
  }
  /**
   * @return bool
   */
  public function getEnableLogging()
  {
    return $this->enableLogging;
  }
  /**
   * @param string[]
   */
  public function setNotificationChannels($notificationChannels)
  {
    $this->notificationChannels = $notificationChannels;
  }
  /**
   * @return string[]
   */
  public function getNotificationChannels()
  {
    return $this->notificationChannels;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ModelMonitoringAlertConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ModelMonitoringAlertConfig');
