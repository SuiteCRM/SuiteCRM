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

class GoogleCloudDiscoveryengineV1SearchResponse extends \Google\Collection
{
  protected $collection_key = 'searchLinkPromotions';
  /**
   * @var string
   */
  public $attributionToken;
  /**
   * @var string
   */
  public $correctedQuery;
  protected $facetsType = GoogleCloudDiscoveryengineV1SearchResponseFacet::class;
  protected $facetsDataType = 'array';
  /**
   * @var string
   */
  public $nextPageToken;
  protected $queryExpansionInfoType = GoogleCloudDiscoveryengineV1SearchResponseQueryExpansionInfo::class;
  protected $queryExpansionInfoDataType = '';
  /**
   * @var string
   */
  public $redirectUri;
  protected $resultsType = GoogleCloudDiscoveryengineV1SearchResponseSearchResult::class;
  protected $resultsDataType = 'array';
  protected $searchLinkPromotionsType = GoogleCloudDiscoveryengineV1SearchLinkPromotion::class;
  protected $searchLinkPromotionsDataType = 'array';
  protected $sessionInfoType = GoogleCloudDiscoveryengineV1SearchResponseSessionInfo::class;
  protected $sessionInfoDataType = '';
  protected $summaryType = GoogleCloudDiscoveryengineV1SearchResponseSummary::class;
  protected $summaryDataType = '';
  /**
   * @var int
   */
  public $totalSize;

  /**
   * @param string
   */
  public function setAttributionToken($attributionToken)
  {
    $this->attributionToken = $attributionToken;
  }
  /**
   * @return string
   */
  public function getAttributionToken()
  {
    return $this->attributionToken;
  }
  /**
   * @param string
   */
  public function setCorrectedQuery($correctedQuery)
  {
    $this->correctedQuery = $correctedQuery;
  }
  /**
   * @return string
   */
  public function getCorrectedQuery()
  {
    return $this->correctedQuery;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchResponseFacet[]
   */
  public function setFacets($facets)
  {
    $this->facets = $facets;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchResponseFacet[]
   */
  public function getFacets()
  {
    return $this->facets;
  }
  /**
   * @param string
   */
  public function setNextPageToken($nextPageToken)
  {
    $this->nextPageToken = $nextPageToken;
  }
  /**
   * @return string
   */
  public function getNextPageToken()
  {
    return $this->nextPageToken;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchResponseQueryExpansionInfo
   */
  public function setQueryExpansionInfo(GoogleCloudDiscoveryengineV1SearchResponseQueryExpansionInfo $queryExpansionInfo)
  {
    $this->queryExpansionInfo = $queryExpansionInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchResponseQueryExpansionInfo
   */
  public function getQueryExpansionInfo()
  {
    return $this->queryExpansionInfo;
  }
  /**
   * @param string
   */
  public function setRedirectUri($redirectUri)
  {
    $this->redirectUri = $redirectUri;
  }
  /**
   * @return string
   */
  public function getRedirectUri()
  {
    return $this->redirectUri;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchResponseSearchResult[]
   */
  public function setResults($results)
  {
    $this->results = $results;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchResponseSearchResult[]
   */
  public function getResults()
  {
    return $this->results;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchLinkPromotion[]
   */
  public function setSearchLinkPromotions($searchLinkPromotions)
  {
    $this->searchLinkPromotions = $searchLinkPromotions;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchLinkPromotion[]
   */
  public function getSearchLinkPromotions()
  {
    return $this->searchLinkPromotions;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchResponseSessionInfo
   */
  public function setSessionInfo(GoogleCloudDiscoveryengineV1SearchResponseSessionInfo $sessionInfo)
  {
    $this->sessionInfo = $sessionInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchResponseSessionInfo
   */
  public function getSessionInfo()
  {
    return $this->sessionInfo;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchResponseSummary
   */
  public function setSummary(GoogleCloudDiscoveryengineV1SearchResponseSummary $summary)
  {
    $this->summary = $summary;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchResponseSummary
   */
  public function getSummary()
  {
    return $this->summary;
  }
  /**
   * @param int
   */
  public function setTotalSize($totalSize)
  {
    $this->totalSize = $totalSize;
  }
  /**
   * @return int
   */
  public function getTotalSize()
  {
    return $this->totalSize;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1SearchResponse::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1SearchResponse');
