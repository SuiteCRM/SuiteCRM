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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1ImportDocumentsRequest extends \Google\Model
{
  protected $alloyDbSourceType = GoogleCloudDiscoveryengineV1AlloyDbSource::class;
  protected $alloyDbSourceDataType = '';
  /**
   * @var bool
   */
  public $autoGenerateIds;
  protected $bigquerySourceType = GoogleCloudDiscoveryengineV1BigQuerySource::class;
  protected $bigquerySourceDataType = '';
  protected $bigtableSourceType = GoogleCloudDiscoveryengineV1BigtableSource::class;
  protected $bigtableSourceDataType = '';
  protected $cloudSqlSourceType = GoogleCloudDiscoveryengineV1CloudSqlSource::class;
  protected $cloudSqlSourceDataType = '';
  protected $errorConfigType = GoogleCloudDiscoveryengineV1ImportErrorConfig::class;
  protected $errorConfigDataType = '';
  protected $fhirStoreSourceType = GoogleCloudDiscoveryengineV1FhirStoreSource::class;
  protected $fhirStoreSourceDataType = '';
  protected $firestoreSourceType = GoogleCloudDiscoveryengineV1FirestoreSource::class;
  protected $firestoreSourceDataType = '';
  /**
   * @var bool
   */
  public $forceRefreshContent;
  protected $gcsSourceType = GoogleCloudDiscoveryengineV1GcsSource::class;
  protected $gcsSourceDataType = '';
  /**
   * @var string
   */
  public $idField;
  protected $inlineSourceType = GoogleCloudDiscoveryengineV1ImportDocumentsRequestInlineSource::class;
  protected $inlineSourceDataType = '';
  /**
   * @var string
   */
  public $reconciliationMode;
  protected $spannerSourceType = GoogleCloudDiscoveryengineV1SpannerSource::class;
  protected $spannerSourceDataType = '';
  /**
   * @var string
   */
  public $updateMask;

  /**
   * @param GoogleCloudDiscoveryengineV1AlloyDbSource
   */
  public function setAlloyDbSource(GoogleCloudDiscoveryengineV1AlloyDbSource $alloyDbSource)
  {
    $this->alloyDbSource = $alloyDbSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1AlloyDbSource
   */
  public function getAlloyDbSource()
  {
    return $this->alloyDbSource;
  }
  /**
   * @param bool
   */
  public function setAutoGenerateIds($autoGenerateIds)
  {
    $this->autoGenerateIds = $autoGenerateIds;
  }
  /**
   * @return bool
   */
  public function getAutoGenerateIds()
  {
    return $this->autoGenerateIds;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1BigQuerySource
   */
  public function setBigquerySource(GoogleCloudDiscoveryengineV1BigQuerySource $bigquerySource)
  {
    $this->bigquerySource = $bigquerySource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1BigQuerySource
   */
  public function getBigquerySource()
  {
    return $this->bigquerySource;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1BigtableSource
   */
  public function setBigtableSource(GoogleCloudDiscoveryengineV1BigtableSource $bigtableSource)
  {
    $this->bigtableSource = $bigtableSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1BigtableSource
   */
  public function getBigtableSource()
  {
    return $this->bigtableSource;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1CloudSqlSource
   */
  public function setCloudSqlSource(GoogleCloudDiscoveryengineV1CloudSqlSource $cloudSqlSource)
  {
    $this->cloudSqlSource = $cloudSqlSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1CloudSqlSource
   */
  public function getCloudSqlSource()
  {
    return $this->cloudSqlSource;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1ImportErrorConfig
   */
  public function setErrorConfig(GoogleCloudDiscoveryengineV1ImportErrorConfig $errorConfig)
  {
    $this->errorConfig = $errorConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ImportErrorConfig
   */
  public function getErrorConfig()
  {
    return $this->errorConfig;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1FhirStoreSource
   */
  public function setFhirStoreSource(GoogleCloudDiscoveryengineV1FhirStoreSource $fhirStoreSource)
  {
    $this->fhirStoreSource = $fhirStoreSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1FhirStoreSource
   */
  public function getFhirStoreSource()
  {
    return $this->fhirStoreSource;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1FirestoreSource
   */
  public function setFirestoreSource(GoogleCloudDiscoveryengineV1FirestoreSource $firestoreSource)
  {
    $this->firestoreSource = $firestoreSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1FirestoreSource
   */
  public function getFirestoreSource()
  {
    return $this->firestoreSource;
  }
  /**
   * @param bool
   */
  public function setForceRefreshContent($forceRefreshContent)
  {
    $this->forceRefreshContent = $forceRefreshContent;
  }
  /**
   * @return bool
   */
  public function getForceRefreshContent()
  {
    return $this->forceRefreshContent;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1GcsSource
   */
  public function setGcsSource(GoogleCloudDiscoveryengineV1GcsSource $gcsSource)
  {
    $this->gcsSource = $gcsSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1GcsSource
   */
  public function getGcsSource()
  {
    return $this->gcsSource;
  }
  /**
   * @param string
   */
  public function setIdField($idField)
  {
    $this->idField = $idField;
  }
  /**
   * @return string
   */
  public function getIdField()
  {
    return $this->idField;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1ImportDocumentsRequestInlineSource
   */
  public function setInlineSource(GoogleCloudDiscoveryengineV1ImportDocumentsRequestInlineSource $inlineSource)
  {
    $this->inlineSource = $inlineSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ImportDocumentsRequestInlineSource
   */
  public function getInlineSource()
  {
    return $this->inlineSource;
  }
  /**
   * @param string
   */
  public function setReconciliationMode($reconciliationMode)
  {
    $this->reconciliationMode = $reconciliationMode;
  }
  /**
   * @return string
   */
  public function getReconciliationMode()
  {
    return $this->reconciliationMode;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SpannerSource
   */
  public function setSpannerSource(GoogleCloudDiscoveryengineV1SpannerSource $spannerSource)
  {
    $this->spannerSource = $spannerSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SpannerSource
   */
  public function getSpannerSource()
  {
    return $this->spannerSource;
  }
  /**
   * @param string
   */
  public function setUpdateMask($updateMask)
  {
    $this->updateMask = $updateMask;
  }
  /**
   * @return string
   */
  public function getUpdateMask()
  {
    return $this->updateMask;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1ImportDocumentsRequest::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1ImportDocumentsRequest');
