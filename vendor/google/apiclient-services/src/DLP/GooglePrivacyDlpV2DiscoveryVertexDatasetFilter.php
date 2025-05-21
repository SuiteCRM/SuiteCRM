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

class GooglePrivacyDlpV2DiscoveryVertexDatasetFilter extends \Google\Model
{
  protected $collectionType = GooglePrivacyDlpV2VertexDatasetCollection::class;
  protected $collectionDataType = '';
  protected $othersType = GooglePrivacyDlpV2AllOtherResources::class;
  protected $othersDataType = '';
  protected $vertexDatasetResourceReferenceType = GooglePrivacyDlpV2VertexDatasetResourceReference::class;
  protected $vertexDatasetResourceReferenceDataType = '';

  /**
   * @param GooglePrivacyDlpV2VertexDatasetCollection
   */
  public function setCollection(GooglePrivacyDlpV2VertexDatasetCollection $collection)
  {
    $this->collection = $collection;
  }
  /**
   * @return GooglePrivacyDlpV2VertexDatasetCollection
   */
  public function getCollection()
  {
    return $this->collection;
  }
  /**
   * @param GooglePrivacyDlpV2AllOtherResources
   */
  public function setOthers(GooglePrivacyDlpV2AllOtherResources $others)
  {
    $this->others = $others;
  }
  /**
   * @return GooglePrivacyDlpV2AllOtherResources
   */
  public function getOthers()
  {
    return $this->others;
  }
  /**
   * @param GooglePrivacyDlpV2VertexDatasetResourceReference
   */
  public function setVertexDatasetResourceReference(GooglePrivacyDlpV2VertexDatasetResourceReference $vertexDatasetResourceReference)
  {
    $this->vertexDatasetResourceReference = $vertexDatasetResourceReference;
  }
  /**
   * @return GooglePrivacyDlpV2VertexDatasetResourceReference
   */
  public function getVertexDatasetResourceReference()
  {
    return $this->vertexDatasetResourceReference;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2DiscoveryVertexDatasetFilter::class, 'Google_Service_DLP_GooglePrivacyDlpV2DiscoveryVertexDatasetFilter');
