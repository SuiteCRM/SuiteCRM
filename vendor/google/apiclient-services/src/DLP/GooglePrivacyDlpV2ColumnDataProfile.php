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

class GooglePrivacyDlpV2ColumnDataProfile extends \Google\Collection
{
  protected $collection_key = 'otherMatches';
  /**
   * @var string
   */
  public $column;
  protected $columnInfoTypeType = GooglePrivacyDlpV2InfoTypeSummary::class;
  protected $columnInfoTypeDataType = '';
  /**
   * @var string
   */
  public $columnType;
  protected $dataRiskLevelType = GooglePrivacyDlpV2DataRiskLevel::class;
  protected $dataRiskLevelDataType = '';
  /**
   * @var string
   */
  public $datasetId;
  /**
   * @var string
   */
  public $datasetLocation;
  /**
   * @var string
   */
  public $datasetProjectId;
  /**
   * @var string
   */
  public $estimatedNullPercentage;
  /**
   * @var string
   */
  public $estimatedUniquenessScore;
  public $freeTextScore;
  /**
   * @var string
   */
  public $name;
  protected $otherMatchesType = GooglePrivacyDlpV2OtherInfoTypeSummary::class;
  protected $otherMatchesDataType = 'array';
  /**
   * @var string
   */
  public $policyState;
  /**
   * @var string
   */
  public $profileLastGenerated;
  protected $profileStatusType = GooglePrivacyDlpV2ProfileStatus::class;
  protected $profileStatusDataType = '';
  protected $sensitivityScoreType = GooglePrivacyDlpV2SensitivityScore::class;
  protected $sensitivityScoreDataType = '';
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $tableDataProfile;
  /**
   * @var string
   */
  public $tableFullResource;
  /**
   * @var string
   */
  public $tableId;

  /**
   * @param string
   */
  public function setColumn($column)
  {
    $this->column = $column;
  }
  /**
   * @return string
   */
  public function getColumn()
  {
    return $this->column;
  }
  /**
   * @param GooglePrivacyDlpV2InfoTypeSummary
   */
  public function setColumnInfoType(GooglePrivacyDlpV2InfoTypeSummary $columnInfoType)
  {
    $this->columnInfoType = $columnInfoType;
  }
  /**
   * @return GooglePrivacyDlpV2InfoTypeSummary
   */
  public function getColumnInfoType()
  {
    return $this->columnInfoType;
  }
  /**
   * @param string
   */
  public function setColumnType($columnType)
  {
    $this->columnType = $columnType;
  }
  /**
   * @return string
   */
  public function getColumnType()
  {
    return $this->columnType;
  }
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
  public function setDatasetId($datasetId)
  {
    $this->datasetId = $datasetId;
  }
  /**
   * @return string
   */
  public function getDatasetId()
  {
    return $this->datasetId;
  }
  /**
   * @param string
   */
  public function setDatasetLocation($datasetLocation)
  {
    $this->datasetLocation = $datasetLocation;
  }
  /**
   * @return string
   */
  public function getDatasetLocation()
  {
    return $this->datasetLocation;
  }
  /**
   * @param string
   */
  public function setDatasetProjectId($datasetProjectId)
  {
    $this->datasetProjectId = $datasetProjectId;
  }
  /**
   * @return string
   */
  public function getDatasetProjectId()
  {
    return $this->datasetProjectId;
  }
  /**
   * @param string
   */
  public function setEstimatedNullPercentage($estimatedNullPercentage)
  {
    $this->estimatedNullPercentage = $estimatedNullPercentage;
  }
  /**
   * @return string
   */
  public function getEstimatedNullPercentage()
  {
    return $this->estimatedNullPercentage;
  }
  /**
   * @param string
   */
  public function setEstimatedUniquenessScore($estimatedUniquenessScore)
  {
    $this->estimatedUniquenessScore = $estimatedUniquenessScore;
  }
  /**
   * @return string
   */
  public function getEstimatedUniquenessScore()
  {
    return $this->estimatedUniquenessScore;
  }
  public function setFreeTextScore($freeTextScore)
  {
    $this->freeTextScore = $freeTextScore;
  }
  public function getFreeTextScore()
  {
    return $this->freeTextScore;
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
   * @param GooglePrivacyDlpV2OtherInfoTypeSummary[]
   */
  public function setOtherMatches($otherMatches)
  {
    $this->otherMatches = $otherMatches;
  }
  /**
   * @return GooglePrivacyDlpV2OtherInfoTypeSummary[]
   */
  public function getOtherMatches()
  {
    return $this->otherMatches;
  }
  /**
   * @param string
   */
  public function setPolicyState($policyState)
  {
    $this->policyState = $policyState;
  }
  /**
   * @return string
   */
  public function getPolicyState()
  {
    return $this->policyState;
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
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setTableDataProfile($tableDataProfile)
  {
    $this->tableDataProfile = $tableDataProfile;
  }
  /**
   * @return string
   */
  public function getTableDataProfile()
  {
    return $this->tableDataProfile;
  }
  /**
   * @param string
   */
  public function setTableFullResource($tableFullResource)
  {
    $this->tableFullResource = $tableFullResource;
  }
  /**
   * @return string
   */
  public function getTableFullResource()
  {
    return $this->tableFullResource;
  }
  /**
   * @param string
   */
  public function setTableId($tableId)
  {
    $this->tableId = $tableId;
  }
  /**
   * @return string
   */
  public function getTableId()
  {
    return $this->tableId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2ColumnDataProfile::class, 'Google_Service_DLP_GooglePrivacyDlpV2ColumnDataProfile');
