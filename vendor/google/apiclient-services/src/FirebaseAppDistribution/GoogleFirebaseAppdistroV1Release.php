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

namespace Google\Service\FirebaseAppDistribution;

class GoogleFirebaseAppdistroV1Release extends \Google\Model
{
  /**
   * @var string
   */
  public $binaryDownloadUri;
  /**
   * @var string
   */
  public $buildVersion;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $displayVersion;
  /**
   * @var string
   */
  public $firebaseConsoleUri;
  /**
   * @var string
   */
  public $name;
  protected $releaseNotesType = GoogleFirebaseAppdistroV1ReleaseNotes::class;
  protected $releaseNotesDataType = '';
  /**
   * @var string
   */
  public $testingUri;

  /**
   * @param string
   */
  public function setBinaryDownloadUri($binaryDownloadUri)
  {
    $this->binaryDownloadUri = $binaryDownloadUri;
  }
  /**
   * @return string
   */
  public function getBinaryDownloadUri()
  {
    return $this->binaryDownloadUri;
  }
  /**
   * @param string
   */
  public function setBuildVersion($buildVersion)
  {
    $this->buildVersion = $buildVersion;
  }
  /**
   * @return string
   */
  public function getBuildVersion()
  {
    return $this->buildVersion;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDisplayVersion($displayVersion)
  {
    $this->displayVersion = $displayVersion;
  }
  /**
   * @return string
   */
  public function getDisplayVersion()
  {
    return $this->displayVersion;
  }
  /**
   * @param string
   */
  public function setFirebaseConsoleUri($firebaseConsoleUri)
  {
    $this->firebaseConsoleUri = $firebaseConsoleUri;
  }
  /**
   * @return string
   */
  public function getFirebaseConsoleUri()
  {
    return $this->firebaseConsoleUri;
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
   * @param GoogleFirebaseAppdistroV1ReleaseNotes
   */
  public function setReleaseNotes(GoogleFirebaseAppdistroV1ReleaseNotes $releaseNotes)
  {
    $this->releaseNotes = $releaseNotes;
  }
  /**
   * @return GoogleFirebaseAppdistroV1ReleaseNotes
   */
  public function getReleaseNotes()
  {
    return $this->releaseNotes;
  }
  /**
   * @param string
   */
  public function setTestingUri($testingUri)
  {
    $this->testingUri = $testingUri;
  }
  /**
   * @return string
   */
  public function getTestingUri()
  {
    return $this->testingUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleFirebaseAppdistroV1Release::class, 'Google_Service_FirebaseAppDistribution_GoogleFirebaseAppdistroV1Release');
