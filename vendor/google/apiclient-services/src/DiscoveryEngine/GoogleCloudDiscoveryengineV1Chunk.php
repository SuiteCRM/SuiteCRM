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

class GoogleCloudDiscoveryengineV1Chunk extends \Google\Model
{
  protected $chunkMetadataType = GoogleCloudDiscoveryengineV1ChunkChunkMetadata::class;
  protected $chunkMetadataDataType = '';
  /**
   * @var string
   */
  public $content;
  /**
   * @var array[]
   */
  public $derivedStructData;
  protected $documentMetadataType = GoogleCloudDiscoveryengineV1ChunkDocumentMetadata::class;
  protected $documentMetadataDataType = '';
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $name;
  protected $pageSpanType = GoogleCloudDiscoveryengineV1ChunkPageSpan::class;
  protected $pageSpanDataType = '';
  public $relevanceScore;

  /**
   * @param GoogleCloudDiscoveryengineV1ChunkChunkMetadata
   */
  public function setChunkMetadata(GoogleCloudDiscoveryengineV1ChunkChunkMetadata $chunkMetadata)
  {
    $this->chunkMetadata = $chunkMetadata;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ChunkChunkMetadata
   */
  public function getChunkMetadata()
  {
    return $this->chunkMetadata;
  }
  /**
   * @param string
   */
  public function setContent($content)
  {
    $this->content = $content;
  }
  /**
   * @return string
   */
  public function getContent()
  {
    return $this->content;
  }
  /**
   * @param array[]
   */
  public function setDerivedStructData($derivedStructData)
  {
    $this->derivedStructData = $derivedStructData;
  }
  /**
   * @return array[]
   */
  public function getDerivedStructData()
  {
    return $this->derivedStructData;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1ChunkDocumentMetadata
   */
  public function setDocumentMetadata(GoogleCloudDiscoveryengineV1ChunkDocumentMetadata $documentMetadata)
  {
    $this->documentMetadata = $documentMetadata;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ChunkDocumentMetadata
   */
  public function getDocumentMetadata()
  {
    return $this->documentMetadata;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
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
   * @param GoogleCloudDiscoveryengineV1ChunkPageSpan
   */
  public function setPageSpan(GoogleCloudDiscoveryengineV1ChunkPageSpan $pageSpan)
  {
    $this->pageSpan = $pageSpan;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ChunkPageSpan
   */
  public function getPageSpan()
  {
    return $this->pageSpan;
  }
  public function setRelevanceScore($relevanceScore)
  {
    $this->relevanceScore = $relevanceScore;
  }
  public function getRelevanceScore()
  {
    return $this->relevanceScore;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1Chunk::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1Chunk');
