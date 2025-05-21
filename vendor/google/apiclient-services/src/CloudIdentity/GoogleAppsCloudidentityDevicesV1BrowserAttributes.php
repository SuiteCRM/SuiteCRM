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

namespace Google\Service\CloudIdentity;

class GoogleAppsCloudidentityDevicesV1BrowserAttributes extends \Google\Model
{
  protected $chromeBrowserInfoType = GoogleAppsCloudidentityDevicesV1BrowserInfo::class;
  protected $chromeBrowserInfoDataType = '';
  /**
   * @var string
   */
  public $chromeProfileId;
  /**
   * @var string
   */
  public $lastProfileSyncTime;

  /**
   * @param GoogleAppsCloudidentityDevicesV1BrowserInfo
   */
  public function setChromeBrowserInfo(GoogleAppsCloudidentityDevicesV1BrowserInfo $chromeBrowserInfo)
  {
    $this->chromeBrowserInfo = $chromeBrowserInfo;
  }
  /**
   * @return GoogleAppsCloudidentityDevicesV1BrowserInfo
   */
  public function getChromeBrowserInfo()
  {
    return $this->chromeBrowserInfo;
  }
  /**
   * @param string
   */
  public function setChromeProfileId($chromeProfileId)
  {
    $this->chromeProfileId = $chromeProfileId;
  }
  /**
   * @return string
   */
  public function getChromeProfileId()
  {
    return $this->chromeProfileId;
  }
  /**
   * @param string
   */
  public function setLastProfileSyncTime($lastProfileSyncTime)
  {
    $this->lastProfileSyncTime = $lastProfileSyncTime;
  }
  /**
   * @return string
   */
  public function getLastProfileSyncTime()
  {
    return $this->lastProfileSyncTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsCloudidentityDevicesV1BrowserAttributes::class, 'Google_Service_CloudIdentity_GoogleAppsCloudidentityDevicesV1BrowserAttributes');
