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

class GooglePrivacyDlpV2DiscoveryOtherCloudFilter extends \Google\Model
{
  protected $collectionType = GooglePrivacyDlpV2OtherCloudResourceCollection::class;
  protected $collectionDataType = '';
  protected $othersType = GooglePrivacyDlpV2AllOtherResources::class;
  protected $othersDataType = '';
  protected $singleResourceType = GooglePrivacyDlpV2OtherCloudSingleResourceReference::class;
  protected $singleResourceDataType = '';

  /**
   * @param GooglePrivacyDlpV2OtherCloudResourceCollection
   */
  public function setCollection(GooglePrivacyDlpV2OtherCloudResourceCollection $collection)
  {
    $this->collection = $collection;
  }
  /**
   * @return GooglePrivacyDlpV2OtherCloudResourceCollection
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
   * @param GooglePrivacyDlpV2OtherCloudSingleResourceReference
   */
  public function setSingleResource(GooglePrivacyDlpV2OtherCloudSingleResourceReference $singleResource)
  {
    $this->singleResource = $singleResource;
  }
  /**
   * @return GooglePrivacyDlpV2OtherCloudSingleResourceReference
   */
  public function getSingleResource()
  {
    return $this->singleResource;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GooglePrivacyDlpV2DiscoveryOtherCloudFilter::class, 'Google_Service_DLP_GooglePrivacyDlpV2DiscoveryOtherCloudFilter');
