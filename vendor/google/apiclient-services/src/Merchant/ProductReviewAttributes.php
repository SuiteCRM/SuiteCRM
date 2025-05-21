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

namespace Google\Service\Merchant;

class ProductReviewAttributes extends \Google\Collection
{
  protected $collection_key = 'skus';
  /**
   * @var string
   */
  public $aggregatorName;
  /**
   * @var string[]
   */
  public $asins;
  /**
   * @var string[]
   */
  public $brands;
  /**
   * @var string
   */
  public $collectionMethod;
  /**
   * @var string[]
   */
  public $cons;
  /**
   * @var string
   */
  public $content;
  /**
   * @var string[]
   */
  public $gtins;
  /**
   * @var bool
   */
  public $isSpam;
  /**
   * @var string
   */
  public $maxRating;
  /**
   * @var string
   */
  public $minRating;
  /**
   * @var string[]
   */
  public $mpns;
  /**
   * @var string[]
   */
  public $productLinks;
  /**
   * @var string[]
   */
  public $productNames;
  /**
   * @var string[]
   */
  public $pros;
  /**
   * @var string
   */
  public $publisherFavicon;
  /**
   * @var string
   */
  public $publisherName;
  public $rating;
  /**
   * @var string
   */
  public $reviewCountry;
  /**
   * @var string
   */
  public $reviewLanguage;
  protected $reviewLinkType = ReviewLink::class;
  protected $reviewLinkDataType = '';
  /**
   * @var string
   */
  public $reviewTime;
  /**
   * @var string
   */
  public $reviewerId;
  /**
   * @var string[]
   */
  public $reviewerImageLinks;
  /**
   * @var bool
   */
  public $reviewerIsAnonymous;
  /**
   * @var string
   */
  public $reviewerUsername;
  /**
   * @var string[]
   */
  public $skus;
  /**
   * @var string
   */
  public $subclientName;
  /**
   * @var string
   */
  public $title;
  /**
   * @var string
   */
  public $transactionId;

  /**
   * @param string
   */
  public function setAggregatorName($aggregatorName)
  {
    $this->aggregatorName = $aggregatorName;
  }
  /**
   * @return string
   */
  public function getAggregatorName()
  {
    return $this->aggregatorName;
  }
  /**
   * @param string[]
   */
  public function setAsins($asins)
  {
    $this->asins = $asins;
  }
  /**
   * @return string[]
   */
  public function getAsins()
  {
    return $this->asins;
  }
  /**
   * @param string[]
   */
  public function setBrands($brands)
  {
    $this->brands = $brands;
  }
  /**
   * @return string[]
   */
  public function getBrands()
  {
    return $this->brands;
  }
  /**
   * @param string
   */
  public function setCollectionMethod($collectionMethod)
  {
    $this->collectionMethod = $collectionMethod;
  }
  /**
   * @return string
   */
  public function getCollectionMethod()
  {
    return $this->collectionMethod;
  }
  /**
   * @param string[]
   */
  public function setCons($cons)
  {
    $this->cons = $cons;
  }
  /**
   * @return string[]
   */
  public function getCons()
  {
    return $this->cons;
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
   * @param string[]
   */
  public function setGtins($gtins)
  {
    $this->gtins = $gtins;
  }
  /**
   * @return string[]
   */
  public function getGtins()
  {
    return $this->gtins;
  }
  /**
   * @param bool
   */
  public function setIsSpam($isSpam)
  {
    $this->isSpam = $isSpam;
  }
  /**
   * @return bool
   */
  public function getIsSpam()
  {
    return $this->isSpam;
  }
  /**
   * @param string
   */
  public function setMaxRating($maxRating)
  {
    $this->maxRating = $maxRating;
  }
  /**
   * @return string
   */
  public function getMaxRating()
  {
    return $this->maxRating;
  }
  /**
   * @param string
   */
  public function setMinRating($minRating)
  {
    $this->minRating = $minRating;
  }
  /**
   * @return string
   */
  public function getMinRating()
  {
    return $this->minRating;
  }
  /**
   * @param string[]
   */
  public function setMpns($mpns)
  {
    $this->mpns = $mpns;
  }
  /**
   * @return string[]
   */
  public function getMpns()
  {
    return $this->mpns;
  }
  /**
   * @param string[]
   */
  public function setProductLinks($productLinks)
  {
    $this->productLinks = $productLinks;
  }
  /**
   * @return string[]
   */
  public function getProductLinks()
  {
    return $this->productLinks;
  }
  /**
   * @param string[]
   */
  public function setProductNames($productNames)
  {
    $this->productNames = $productNames;
  }
  /**
   * @return string[]
   */
  public function getProductNames()
  {
    return $this->productNames;
  }
  /**
   * @param string[]
   */
  public function setPros($pros)
  {
    $this->pros = $pros;
  }
  /**
   * @return string[]
   */
  public function getPros()
  {
    return $this->pros;
  }
  /**
   * @param string
   */
  public function setPublisherFavicon($publisherFavicon)
  {
    $this->publisherFavicon = $publisherFavicon;
  }
  /**
   * @return string
   */
  public function getPublisherFavicon()
  {
    return $this->publisherFavicon;
  }
  /**
   * @param string
   */
  public function setPublisherName($publisherName)
  {
    $this->publisherName = $publisherName;
  }
  /**
   * @return string
   */
  public function getPublisherName()
  {
    return $this->publisherName;
  }
  public function setRating($rating)
  {
    $this->rating = $rating;
  }
  public function getRating()
  {
    return $this->rating;
  }
  /**
   * @param string
   */
  public function setReviewCountry($reviewCountry)
  {
    $this->reviewCountry = $reviewCountry;
  }
  /**
   * @return string
   */
  public function getReviewCountry()
  {
    return $this->reviewCountry;
  }
  /**
   * @param string
   */
  public function setReviewLanguage($reviewLanguage)
  {
    $this->reviewLanguage = $reviewLanguage;
  }
  /**
   * @return string
   */
  public function getReviewLanguage()
  {
    return $this->reviewLanguage;
  }
  /**
   * @param ReviewLink
   */
  public function setReviewLink(ReviewLink $reviewLink)
  {
    $this->reviewLink = $reviewLink;
  }
  /**
   * @return ReviewLink
   */
  public function getReviewLink()
  {
    return $this->reviewLink;
  }
  /**
   * @param string
   */
  public function setReviewTime($reviewTime)
  {
    $this->reviewTime = $reviewTime;
  }
  /**
   * @return string
   */
  public function getReviewTime()
  {
    return $this->reviewTime;
  }
  /**
   * @param string
   */
  public function setReviewerId($reviewerId)
  {
    $this->reviewerId = $reviewerId;
  }
  /**
   * @return string
   */
  public function getReviewerId()
  {
    return $this->reviewerId;
  }
  /**
   * @param string[]
   */
  public function setReviewerImageLinks($reviewerImageLinks)
  {
    $this->reviewerImageLinks = $reviewerImageLinks;
  }
  /**
   * @return string[]
   */
  public function getReviewerImageLinks()
  {
    return $this->reviewerImageLinks;
  }
  /**
   * @param bool
   */
  public function setReviewerIsAnonymous($reviewerIsAnonymous)
  {
    $this->reviewerIsAnonymous = $reviewerIsAnonymous;
  }
  /**
   * @return bool
   */
  public function getReviewerIsAnonymous()
  {
    return $this->reviewerIsAnonymous;
  }
  /**
   * @param string
   */
  public function setReviewerUsername($reviewerUsername)
  {
    $this->reviewerUsername = $reviewerUsername;
  }
  /**
   * @return string
   */
  public function getReviewerUsername()
  {
    return $this->reviewerUsername;
  }
  /**
   * @param string[]
   */
  public function setSkus($skus)
  {
    $this->skus = $skus;
  }
  /**
   * @return string[]
   */
  public function getSkus()
  {
    return $this->skus;
  }
  /**
   * @param string
   */
  public function setSubclientName($subclientName)
  {
    $this->subclientName = $subclientName;
  }
  /**
   * @return string
   */
  public function getSubclientName()
  {
    return $this->subclientName;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
  /**
   * @param string
   */
  public function setTransactionId($transactionId)
  {
    $this->transactionId = $transactionId;
  }
  /**
   * @return string
   */
  public function getTransactionId()
  {
    return $this->transactionId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProductReviewAttributes::class, 'Google_Service_Merchant_ProductReviewAttributes');
