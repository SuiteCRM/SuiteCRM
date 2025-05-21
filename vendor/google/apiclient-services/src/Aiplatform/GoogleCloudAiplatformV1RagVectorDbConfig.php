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

class GoogleCloudAiplatformV1RagVectorDbConfig extends \Google\Model
{
  protected $apiAuthType = GoogleCloudAiplatformV1ApiAuth::class;
  protected $apiAuthDataType = '';
  protected $pineconeType = GoogleCloudAiplatformV1RagVectorDbConfigPinecone::class;
  protected $pineconeDataType = '';
  protected $ragEmbeddingModelConfigType = GoogleCloudAiplatformV1RagEmbeddingModelConfig::class;
  protected $ragEmbeddingModelConfigDataType = '';
  protected $ragManagedDbType = GoogleCloudAiplatformV1RagVectorDbConfigRagManagedDb::class;
  protected $ragManagedDbDataType = '';
  protected $vertexVectorSearchType = GoogleCloudAiplatformV1RagVectorDbConfigVertexVectorSearch::class;
  protected $vertexVectorSearchDataType = '';

  /**
   * @param GoogleCloudAiplatformV1ApiAuth
   */
  public function setApiAuth(GoogleCloudAiplatformV1ApiAuth $apiAuth)
  {
    $this->apiAuth = $apiAuth;
  }
  /**
   * @return GoogleCloudAiplatformV1ApiAuth
   */
  public function getApiAuth()
  {
    return $this->apiAuth;
  }
  /**
   * @param GoogleCloudAiplatformV1RagVectorDbConfigPinecone
   */
  public function setPinecone(GoogleCloudAiplatformV1RagVectorDbConfigPinecone $pinecone)
  {
    $this->pinecone = $pinecone;
  }
  /**
   * @return GoogleCloudAiplatformV1RagVectorDbConfigPinecone
   */
  public function getPinecone()
  {
    return $this->pinecone;
  }
  /**
   * @param GoogleCloudAiplatformV1RagEmbeddingModelConfig
   */
  public function setRagEmbeddingModelConfig(GoogleCloudAiplatformV1RagEmbeddingModelConfig $ragEmbeddingModelConfig)
  {
    $this->ragEmbeddingModelConfig = $ragEmbeddingModelConfig;
  }
  /**
   * @return GoogleCloudAiplatformV1RagEmbeddingModelConfig
   */
  public function getRagEmbeddingModelConfig()
  {
    return $this->ragEmbeddingModelConfig;
  }
  /**
   * @param GoogleCloudAiplatformV1RagVectorDbConfigRagManagedDb
   */
  public function setRagManagedDb(GoogleCloudAiplatformV1RagVectorDbConfigRagManagedDb $ragManagedDb)
  {
    $this->ragManagedDb = $ragManagedDb;
  }
  /**
   * @return GoogleCloudAiplatformV1RagVectorDbConfigRagManagedDb
   */
  public function getRagManagedDb()
  {
    return $this->ragManagedDb;
  }
  /**
   * @param GoogleCloudAiplatformV1RagVectorDbConfigVertexVectorSearch
   */
  public function setVertexVectorSearch(GoogleCloudAiplatformV1RagVectorDbConfigVertexVectorSearch $vertexVectorSearch)
  {
    $this->vertexVectorSearch = $vertexVectorSearch;
  }
  /**
   * @return GoogleCloudAiplatformV1RagVectorDbConfigVertexVectorSearch
   */
  public function getVertexVectorSearch()
  {
    return $this->vertexVectorSearch;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1RagVectorDbConfig::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1RagVectorDbConfig');
