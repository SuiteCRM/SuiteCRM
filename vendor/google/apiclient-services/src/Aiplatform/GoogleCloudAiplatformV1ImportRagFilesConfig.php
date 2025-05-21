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

class GoogleCloudAiplatformV1ImportRagFilesConfig extends \Google\Model
{
  protected $gcsSourceType = GoogleCloudAiplatformV1GcsSource::class;
  protected $gcsSourceDataType = '';
  protected $googleDriveSourceType = GoogleCloudAiplatformV1GoogleDriveSource::class;
  protected $googleDriveSourceDataType = '';
  protected $jiraSourceType = GoogleCloudAiplatformV1JiraSource::class;
  protected $jiraSourceDataType = '';
  /**
   * @var int
   */
  public $maxEmbeddingRequestsPerMin;
  protected $partialFailureBigquerySinkType = GoogleCloudAiplatformV1BigQueryDestination::class;
  protected $partialFailureBigquerySinkDataType = '';
  protected $partialFailureGcsSinkType = GoogleCloudAiplatformV1GcsDestination::class;
  protected $partialFailureGcsSinkDataType = '';
  protected $ragFileTransformationConfigType = GoogleCloudAiplatformV1RagFileTransformationConfig::class;
  protected $ragFileTransformationConfigDataType = '';
  protected $sharePointSourcesType = GoogleCloudAiplatformV1SharePointSources::class;
  protected $sharePointSourcesDataType = '';
  protected $slackSourceType = GoogleCloudAiplatformV1SlackSource::class;
  protected $slackSourceDataType = '';

  /**
   * @param GoogleCloudAiplatformV1GcsSource
   */
  public function setGcsSource(GoogleCloudAiplatformV1GcsSource $gcsSource)
  {
    $this->gcsSource = $gcsSource;
  }
  /**
   * @return GoogleCloudAiplatformV1GcsSource
   */
  public function getGcsSource()
  {
    return $this->gcsSource;
  }
  /**
   * @param GoogleCloudAiplatformV1GoogleDriveSource
   */
  public function setGoogleDriveSource(GoogleCloudAiplatformV1GoogleDriveSource $googleDriveSource)
  {
    $this->googleDriveSource = $googleDriveSource;
  }
  /**
   * @return GoogleCloudAiplatformV1GoogleDriveSource
   */
  public function getGoogleDriveSource()
  {
    return $this->googleDriveSource;
  }
  /**
   * @param GoogleCloudAiplatformV1JiraSource
   */
  public function setJiraSource(GoogleCloudAiplatformV1JiraSource $jiraSource)
  {
    $this->jiraSource = $jiraSource;
  }
  /**
   * @return GoogleCloudAiplatformV1JiraSource
   */
  public function getJiraSource()
  {
    return $this->jiraSource;
  }
  /**
   * @param int
   */
  public function setMaxEmbeddingRequestsPerMin($maxEmbeddingRequestsPerMin)
  {
    $this->maxEmbeddingRequestsPerMin = $maxEmbeddingRequestsPerMin;
  }
  /**
   * @return int
   */
  public function getMaxEmbeddingRequestsPerMin()
  {
    return $this->maxEmbeddingRequestsPerMin;
  }
  /**
   * @param GoogleCloudAiplatformV1BigQueryDestination
   */
  public function setPartialFailureBigquerySink(GoogleCloudAiplatformV1BigQueryDestination $partialFailureBigquerySink)
  {
    $this->partialFailureBigquerySink = $partialFailureBigquerySink;
  }
  /**
   * @return GoogleCloudAiplatformV1BigQueryDestination
   */
  public function getPartialFailureBigquerySink()
  {
    return $this->partialFailureBigquerySink;
  }
  /**
   * @param GoogleCloudAiplatformV1GcsDestination
   */
  public function setPartialFailureGcsSink(GoogleCloudAiplatformV1GcsDestination $partialFailureGcsSink)
  {
    $this->partialFailureGcsSink = $partialFailureGcsSink;
  }
  /**
   * @return GoogleCloudAiplatformV1GcsDestination
   */
  public function getPartialFailureGcsSink()
  {
    return $this->partialFailureGcsSink;
  }
  /**
   * @param GoogleCloudAiplatformV1RagFileTransformationConfig
   */
  public function setRagFileTransformationConfig(GoogleCloudAiplatformV1RagFileTransformationConfig $ragFileTransformationConfig)
  {
    $this->ragFileTransformationConfig = $ragFileTransformationConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1RagFileTransformationConfig
   */
  public function getRagFileTransformationConfig()
  {
    return $this->ragFileTransformationConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1SharePointSources
   */
  public function setSharePointSources(GoogleCloudAiplatformV1SharePointSources $sharePointSources)
  {
    $this->sharePointSources = $sharePointSources;
  }
  /**
   * @return GoogleCloudAiplatformV1SharePointSources
   */
  public function getSharePointSources()
  {
    return $this->sharePointSources;
  }
  /**
   * @param GoogleCloudAiplatformV1SlackSource
   */
  public function setSlackSource(GoogleCloudAiplatformV1SlackSource $slackSource)
  {
    $this->slackSource = $slackSource;
  }
  /**
   * @return GoogleCloudAiplatformV1SlackSource
   */
  public function getSlackSource()
  {
    return $this->slackSource;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ImportRagFilesConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ImportRagFilesConfig');
