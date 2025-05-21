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

class GooglePrivacyDlpV2FileStoreDataProfile extends \Google\Collection
{
  protected $collection_key = 'relatedResources';
  protected $configSnapshotType = GooglePrivacyDlpV2DataProfileConfigSnapshot::class;
  protected $configSnapshotDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $dataRiskLevelType = GooglePrivacyDlpV2DataRiskLevel::class;
  protected $dataRiskLevelDataType = '';
  protected $dataSourceTypeType = GooglePrivacyDlpV2DataSourceType::class;
  protected $dataSourceTypeDataType = '';
  /**
   * @var string[]
   */
  public $dataStorageLocations;
  protected $fileClusterSummariesType = GooglePrivacyDlpV2FileClusterSummary::class;
  protected $fileClusterSummariesDataType = 'array';
  protected $fileStoreInfoTypeSummariesType = GooglePrivacyDlpV2FileStoreInfoTypeSummary::class;
  protected $fileStoreInfoTypeSummariesDataType = 'array';
  /**
   * @var bool
   */
  public $fileStoreIsEmpty;
  /**
   * @var string
   */
  public $fileStoreLocation;
  /**
   * @var string
   */
  public $fileStorePath;
  /**
   * @var string
   */
  public $fullResource;
  /**
   * @var string
   */
  public $lastModifiedTime;
  /**
   * @var string
   */
  public $locationType;
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
  public $projectDataProfile;
  /**
   * @var string
   */
  public $projectId;
  protected $relatedResourcesType = GooglePrivacyDlpV2RelatedResource::class;
  protected $relatedResourcesDataType = 'array';
  protected $resourceAttributesType = GooglePrivacyDlpV2Value::class;
  protected $resourceAttributesDataType = 'map';
  /**
   * @var string[]
   */
  public $resourceLabels;
  /**
   * @var string
   */
  public $resourceVisibility;
  protected $sensitivityScoreType = GooglePrivacyDlpV2SensitivityScore::class;
  protected $sensitivityScoreDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param GooglePrivacyDlpV2DataProfileConfigSnapshot
   */
  public function setConfigSnapshot(GooglePrivacyDlpV2DataProfileConfigSnapshot $configSnapshot)
  {
    $this->configSnapshot = $configSnapshot;
  }
  /**
   * @return GooglePrivacyDlpV2DataProfileConfigSnapshot
   */
  public function getConfigSnapshot()
  {
    return $this->configSnapshot;
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
   * @param GooglePrivacyDlpV2DataSourceType
   */
  public function setDataSourceType(GooglePrivacyDlpV2DataSourceType $dataSourceType)
  {
    $this->dataSourceType = $dataSourceType;
  }
  /**
   * @return GooglePrivacyDlpV2DataSourceType
   */
  public function getDataSourceType()
  {
    return $this->dataSourceType;
  }
  /**
   * @param string[]
   */
  public function setDataStorageLocations($dataStorageLocations)
  {
    $this->dataStorageLocations = $dataStorageLocations;
  }
  /**
   * @return string[]
   */
  public function getDataStorageLocations()
  {
    return $this->dataStorageLocations;
  }
  /**
   * @param GooglePrivacyDlpV2FileClusterSummary[]
   */
  public function setFileClusterSummaries($fileClusterSummaries)
  {
    $this->fileClusterSummaries = $fileClusterSummaries;
  }
  /**
   * @return GooglePrivacyDlpV2FileClusterSummary[]
   */
  public function getFileClusterSummaries()
  {
    return $this->fileClusterSummaries;
  }
  /**
   * @param GooglePrivacyDlpV2FileStoreInfoTypeSummary[]
   */
  public function setFileStoreInfoTypeSummaries($fileStoreInfoTypeSummaries)
  {
    $this->fileStoreInfoTypeSummaries = $fileStoreInfoTypeSummaries;
  }
  /**
   * @return GooglePrivacyDlpV2FileStoreInfoTypeSummary[]
   */
  public function getFileStoreInfoTypeSummaries()
  {
    return $this->fileStoreInfoTypeSummaries;
  }
  /**
   * @param bool
   */
  public function setFileStoreIsEmpty($fileStoreIsEmpty)
  {
    $this->fileStoreIsEmpty = $fileStoreIsEmpty;
  }
  /**
   * @return bool
   */
  public function getFileStoreIsEmpty()
  {
    return $this->fileStoreIsEmpty;
  }
  /**
   * @param string
   */
  public function setFileStoreLocation($fileStoreLocation)
  {
    $this->fileStoreLocation = $fileStoreLocation;
  }
  /**
   * @return string
   */
  public function getFileStoreLocation()
  {
    return $this->fileStoreLocation;
  }
  /**
   * @param string
   */
  public function setFileStorePath($fileStorePath)
  {
    $this->fileStorePath = $fileStorePath;
  }
  /**
   * @return string
   */
  public function getFileStorePath()
  {
    return $this->fileStorePath;
  }
  /**
   * @param string
   */
  public function setFullResource($fullResource)
  {
    $this->fullResource = $fullResource;
  }
  /**
   * @return string
   */
  public function getFullResource()
  {
    return $this->fullResource;
  }
  /**
   * @param string
   */
  public function setLastModifiedTime($lastModifiedTime)
  {
    $this->lastModifiedTime = $lastModifiedTime;
  }
  /**
   * @return string
   */
  public function getLastModifiedTime()
  {
    return $this->lastModifiedTime;
  }
  /**
   * @param string
   */
  public function setLocationType($locationType)
  {
    $this->locationType = $locationType;
  }
  /**
   * @return string
   */
  public function getLocationType()
  {
    return $this->locationType;
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
  public function setProjectDataProfile($projectDataProfile)
  {
    $this->projectDataProfile = $projectDataProfile;
  }
  /**
   * @return string
   */
  public function getProjectDataProfile()
  {
    return $this->projectDataProfile;
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
   * @param GooglePrivacyDlpV2RelatedResource[]
   */
  public function setRelatedResources($relatedResources)
  {
    $this->relatedResources = $relatedResources;
  }
  /**
   * @return GooglePrivacyDlpV2RelatedResource[]
   */
  public function getRelatedResources()
  {
    return $this->relatedResources;
  }
  /**
   * @param GooglePrivacyDlpV2Value[]
   */
  public function setResourceAttributes($resourceAttributes)
  {
    $this->resourceAttributes = $resourceAttributes;
  }
  /**
   * @return GooglePrivacyDlpV2Value[]
   */
  public function getResourceAttributes()
  {
    return $this->resourceAttributes;
  }
  /**
   * @param string[]
   */
  public function setResourceLabels($resourceLabels)
  {
    $this->resourceLabels = $resourceLabels;
  }
  /**
   * @return string[]
   */
  public function getResourceLabels()
  {
    return $this->resourceLabels;
  }
  /**
   * @param string
   */
  public function setResourceVisibility($resourceVisibility)
  {
    $this->resourceVisibility = $resourceVisibility;
  }
  /**
   * @return string
   */
  public function getResourceVisibility()
  {
    return $this->resourceVisibility;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2FileStoreDataProfile::class, 'Google_Service_DLP_GooglePrivacyDlpV2FileStoreDataProfile');
