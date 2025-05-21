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

namespace Google\Service\DLP;

class GooglePrivacyDlpV2ProjectDataProfile extends \Google\Model
{
  protected $dataRiskLevelType = GooglePrivacyDlpV2DataRiskLevel::class;
  protected $dataRiskLevelDataType = '';
  /**
   * @var string
   */
  public $fileStoreDataProfileCount;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $profileLastGenerated;
  protected $profileStatusType = GooglePrivacyDlpV2ProfileStatus::class;
  protected $profileStatusDataType = '';
  /**
   * @var string
   */
  public $projectId;
  protected $sensitivityScoreType = GooglePrivacyDlpV2SensitivityScore::class;
  protected $sensitivityScoreDataType = '';
  /**
   * @var string
   */
  public $tableDataProfileCount;

  /**
   * @param GooglePrivacyDlpV2DataRiskLevel
   */
  public function setDataRiskLevel(GooglePrivacyDlpV2DataRiskLevel $dataRiskLevel)
  {
    $this->dataRiskLevel = $dataRiskLevel;
  }
  /**
   * @return GooglePrivacyDlpV2DataRiskLevel
   */
  public function getDataRiskLevel()
  {
    return $this->dataRiskLevel;
  }
  /**
   * @param string
   */
  public function setFileStoreDataProfileCount($fileStoreDataProfileCount)
  {
    $this->fileStoreDataProfileCount = $fileStoreDataProfileCount;
  }
  /**
   * @return string
   */
  public function getFileStoreDataProfileCount()
  {
    return $this->fileStoreDataProfileCount;
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
   * @param string
   */
  public function setProfileLastGenerated($profileLastGenerated)
  {
    $this->profileLastGenerated = $profileLastGenerated;
  }
  /**
   * @return string
   */
  public function getProfileLastGenerated()
  {
    return $this->profileLastGenerated;
  }
  /**
   * @param GooglePrivacyDlpV2ProfileStatus
   */
  public function setProfileStatus(GooglePrivacyDlpV2ProfileStatus $profileStatus)
  {
    $this->profileStatus = $profileStatus;
  }
  /**
   * @return GooglePrivacyDlpV2ProfileStatus
   */
  public function getProfileStatus()
  {
    return $this->profileStatus;
  }
  /**
   * @param string
   */
  public function setProjectId($projectId)
  {
    $this->projectId = $projectId;
  }
  /**
   * @return string
   */
  public function getProjectId()
  {
    return $this->projectId;
  }
  /**
   * @param GooglePrivacyDlpV2SensitivityScore
   */
  public function setSensitivityScore(GooglePrivacyDlpV2SensitivityScore $sensitivityScore)
  {
    $this->sensitivityScore = $sensitivityScore;
  }
  /**
   * @return GooglePrivacyDlpV2SensitivityScore
   */
  public function getSensitivityScore()
  {
    return $this->sensitivityScore;
  }
  /**
   * @param string
   */
  public function setTableDataProfileCount($tableDataProfileCount)
  {
    $this->tableDataProfileCount = $tableDataProfileCount;
  }
  /**
   * @return string
   */
  public function getTableDataProfileCount()
  {
    return $this->tableDataProfileCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2ProjectDataProfile::class, 'Google_Service_DLP_GooglePrivacyDlpV2ProjectDataProfile');
