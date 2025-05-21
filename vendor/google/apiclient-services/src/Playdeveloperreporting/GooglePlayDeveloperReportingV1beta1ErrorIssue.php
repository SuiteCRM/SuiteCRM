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

class GooglePlayDeveloperReportingV1beta1ErrorIssue extends \Google\Collection
{
  protected $collection_key = 'sampleErrorReports';
  protected $annotationsType = GooglePlayDeveloperReportingV1beta1IssueAnnotation::class;
  protected $annotationsDataType = 'array';
  /**
   * @var string
   */
  public $cause;
  /**
   * @var string
   */
  public $distinctUsers;
  protected $distinctUsersPercentType = GoogleTypeDecimal::class;
  protected $distinctUsersPercentDataType = '';
  /**
   * @var string
   */
  public $errorReportCount;
  protected $firstAppVersionType = GooglePlayDeveloperReportingV1beta1AppVersion::class;
  protected $firstAppVersionDataType = '';
  protected $firstOsVersionType = GooglePlayDeveloperReportingV1beta1OsVersion::class;
  protected $firstOsVersionDataType = '';
  /**
   * @var string
   */
  public $issueUri;
  protected $lastAppVersionType = GooglePlayDeveloperReportingV1beta1AppVersion::class;
  protected $lastAppVersionDataType = '';
  /**
   * @var string
   */
  public $lastErrorReportTime;
  protected $lastOsVersionType = GooglePlayDeveloperReportingV1beta1OsVersion::class;
  protected $lastOsVersionDataType = '';
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $sampleErrorReports;
  /**
   * @var string
   */
  public $type;

  /**
   * @param GooglePlayDeveloperReportingV1beta1IssueAnnotation[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return GooglePlayDeveloperReportingV1beta1IssueAnnotation[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param string
   */
  public function setCause($cause)
  {
    $this->cause = $cause;
  }
  /**
   * @return string
   */
  public function getCause()
  {
    return $this->cause;
  }
  /**
   * @param string
   */
  public function setDistinctUsers($distinctUsers)
  {
    $this->distinctUsers = $distinctUsers;
  }
  /**
   * @return string
   */
  public function getDistinctUsers()
  {
    return $this->distinctUsers;
  }
  /**
   * @param GoogleTypeDecimal
   */
  public function setDistinctUsersPercent(GoogleTypeDecimal $distinctUsersPercent)
  {
    $this->distinctUsersPercent = $distinctUsersPercent;
  }
  /**
   * @return GoogleTypeDecimal
   */
  public function getDistinctUsersPercent()
  {
    return $this->distinctUsersPercent;
  }
  /**
   * @param string
   */
  public function setErrorReportCount($errorReportCount)
  {
    $this->errorReportCount = $errorReportCount;
  }
  /**
   * @return string
   */
  public function getErrorReportCount()
  {
    return $this->errorReportCount;
  }
  /**
   * @param GooglePlayDeveloperReportingV1beta1AppVersion
   */
  public function setFirstAppVersion(GooglePlayDeveloperReportingV1beta1AppVersion $firstAppVersion)
  {
    $this->firstAppVersion = $firstAppVersion;
  }
  /**
   * @return GooglePlayDeveloperReportingV1beta1AppVersion
   */
  public function getFirstAppVersion()
  {
    return $this->firstAppVersion;
  }
  /**
   * @param GooglePlayDeveloperReportingV1beta1OsVersion
   */
  public function setFirstOsVersion(GooglePlayDeveloperReportingV1beta1OsVersion $firstOsVersion)
  {
    $this->firstOsVersion = $firstOsVersion;
  }
  /**
   * @return GooglePlayDeveloperReportingV1beta1OsVersion
   */
  public function getFirstOsVersion()
  {
    return $this->firstOsVersion;
  }
  /**
   * @param string
   */
  public function setIssueUri($issueUri)
  {
    $this->issueUri = $issueUri;
  }
  /**
   * @return string
   */
  public function getIssueUri()
  {
    return $this->issueUri;
  }
  /**
   * @param GooglePlayDeveloperReportingV1beta1AppVersion
   */
  public function setLastAppVersion(GooglePlayDeveloperReportingV1beta1AppVersion $lastAppVersion)
  {
    $this->lastAppVersion = $lastAppVersion;
  }
  /**
   * @return GooglePlayDeveloperReportingV1beta1AppVersion
   */
  public function getLastAppVersion()
  {
    return $this->lastAppVersion;
  }
  /**
   * @param string
   */
  public function setLastErrorReportTime($lastErrorReportTime)
  {
    $this->lastErrorReportTime = $lastErrorReportTime;
  }
  /**
   * @return string
   */
  public function getLastErrorReportTime()
  {
    return $this->lastErrorReportTime;
  }
  /**
   * @param GooglePlayDeveloperReportingV1beta1OsVersion
   */
  public function setLastOsVersion(GooglePlayDeveloperReportingV1beta1OsVersion $lastOsVersion)
  {
    $this->lastOsVersion = $lastOsVersion;
  }
  /**
   * @return GooglePlayDeveloperReportingV1beta1OsVersion
   */
  public function getLastOsVersion()
  {
    return $this->lastOsVersion;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
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
   * @param string[]
   */
  public function setSampleErrorReports($sampleErrorReports)
  {
    $this->sampleErrorReports = $sampleErrorReports;
  }
  /**
   * @return string[]
   */
  public function getSampleErrorReports()
  {
    return $this->sampleErrorReports;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePlayDeveloperReportingV1beta1ErrorIssue::class, 'Google_Service_Playdeveloperreporting_GooglePlayDeveloperReportingV1beta1ErrorIssue');
