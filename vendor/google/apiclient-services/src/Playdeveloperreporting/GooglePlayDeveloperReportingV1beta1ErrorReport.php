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

namespace Google\Service\Playdeveloperreporting;

class GooglePlayDeveloperReportingV1beta1ErrorReport extends \Google\Model
{
  protected $appVersionType = GooglePlayDeveloperReportingV1beta1AppVersion::class;
  protected $appVersionDataType = '';
  protected $deviceModelType = GooglePlayDeveloperReportingV1beta1DeviceModelSummary::class;
  protected $deviceModelDataType = '';
  /**
   * @var string
   */
  public $eventTime;
  /**
   * @var string
   */
  public $issue;
  /**
   * @var string
   */
  public $name;
  protected $osVersionType = GooglePlayDeveloperReportingV1beta1OsVersion::class;
  protected $osVersionDataType = '';
  /**
   * @var string
   */
  public $reportText;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $vcsInformation;

  /**
   * @param GooglePlayDeveloperReportingV1beta1AppVersion
   */
  public function setAppVersion(GooglePlayDeveloperReportingV1beta1AppVersion $appVersion)
  {
    $this->appVersion = $appVersion;
  }
  /**
   * @return GooglePlayDeveloperReportingV1beta1AppVersion
   */
  public function getAppVersion()
  {
    return $this->appVersion;
  }
  /**
   * @param GooglePlayDeveloperReportingV1beta1DeviceModelSummary
   */
  public function setDeviceModel(GooglePlayDeveloperReportingV1beta1DeviceModelSummary $deviceModel)
  {
    $this->deviceModel = $deviceModel;
  }
  /**
   * @return GooglePlayDeveloperReportingV1beta1DeviceModelSummary
   */
  public function getDeviceModel()
  {
    return $this->deviceModel;
  }
  /**
   * @param string
   */
  public function setEventTime($eventTime)
  {
    $this->eventTime = $eventTime;
  }
  /**
   * @return string
   */
  public function getEventTime()
  {
    return $this->eventTime;
  }
  /**
   * @param string
   */
  public function setIssue($issue)
  {
    $this->issue = $issue;
  }
  /**
   * @return string
   */
  public function getIssue()
  {
    return $this->issue;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param GooglePlayDeveloperReportingV1beta1OsVersion
   */
  public function setOsVersion(GooglePlayDeveloperReportingV1beta1OsVersion $osVersion)
  {
    $this->osVersion = $osVersion;
  }
  /**
   * @return GooglePlayDeveloperReportingV1beta1OsVersion
   */
  public function getOsVersion()
  {
    return $this->osVersion;
  }
  /**
   * @param string
   */
  public function setReportText($reportText)
  {
    $this->reportText = $reportText;
  }
  /**
   * @return string
   */
  public function getReportText()
  {
    return $this->reportText;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param string
   */
  public function setVcsInformation($vcsInformation)
  {
    $this->vcsInformation = $vcsInformation;
  }
  /**
   * @return string
   */
  public function getVcsInformation()
  {
    return $this->vcsInformation;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePlayDeveloperReportingV1beta1ErrorReport::class, 'Google_Service_Playdeveloperreporting_GooglePlayDeveloperReportingV1beta1ErrorReport');
