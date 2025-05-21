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

class GooglePrivacyDlpV2FileClusterSummary extends \Google\Collection
{
  protected $collection_key = 'fileStoreInfoTypeSummaries';
  protected $dataRiskLevelType = GooglePrivacyDlpV2DataRiskLevel::class;
  protected $dataRiskLevelDataType = '';
  protected $errorsType = GooglePrivacyDlpV2Error::class;
  protected $errorsDataType = 'array';
  protected $fileClusterTypeType = GooglePrivacyDlpV2FileClusterType::class;
  protected $fileClusterTypeDataType = '';
  protected $fileExtensionsScannedType = GooglePrivacyDlpV2FileExtensionInfo::class;
  protected $fileExtensionsScannedDataType = 'array';
  protected $fileExtensionsSeenType = GooglePrivacyDlpV2FileExtensionInfo::class;
  protected $fileExtensionsSeenDataType = 'array';
  protected $fileStoreInfoTypeSummariesType = GooglePrivacyDlpV2FileStoreInfoTypeSummary::class;
  protected $fileStoreInfoTypeSummariesDataType = 'array';
  /**
   * @var bool
   */
  public $noFilesExist;
  protected $sensitivityScoreType = GooglePrivacyDlpV2SensitivityScore::class;
  protected $sensitivityScoreDataType = '';

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
   * @param GooglePrivacyDlpV2Error[]
   */
  public function setErrors($errors)
  {
    $this->errors = $errors;
  }
  /**
   * @return GooglePrivacyDlpV2Error[]
   */
  public function getErrors()
  {
    return $this->errors;
  }
  /**
   * @param GooglePrivacyDlpV2FileClusterType
   */
  public function setFileClusterType(GooglePrivacyDlpV2FileClusterType $fileClusterType)
  {
    $this->fileClusterType = $fileClusterType;
  }
  /**
   * @return GooglePrivacyDlpV2FileClusterType
   */
  public function getFileClusterType()
  {
    return $this->fileClusterType;
  }
  /**
   * @param GooglePrivacyDlpV2FileExtensionInfo[]
   */
  public function setFileExtensionsScanned($fileExtensionsScanned)
  {
    $this->fileExtensionsScanned = $fileExtensionsScanned;
  }
  /**
   * @return GooglePrivacyDlpV2FileExtensionInfo[]
   */
  public function getFileExtensionsScanned()
  {
    return $this->fileExtensionsScanned;
  }
  /**
   * @param GooglePrivacyDlpV2FileExtensionInfo[]
   */
  public function setFileExtensionsSeen($fileExtensionsSeen)
  {
    $this->fileExtensionsSeen = $fileExtensionsSeen;
  }
  /**
   * @return GooglePrivacyDlpV2FileExtensionInfo[]
   */
  public function getFileExtensionsSeen()
  {
    return $this->fileExtensionsSeen;
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
  public function setNoFilesExist($noFilesExist)
  {
    $this->noFilesExist = $noFilesExist;
  }
  /**
   * @return bool
   */
  public function getNoFilesExist()
  {
    return $this->noFilesExist;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2FileClusterSummary::class, 'Google_Service_DLP_GooglePrivacyDlpV2FileClusterSummary');
