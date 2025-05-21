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

class MerchantReviewAttributes extends \Google\Model
{
  /**
   * @var string
   */
  public $collectionMethod;
  /**
   * @var string
   */
  public $content;
  /**
   * @var bool
   */
  public $isAnonymous;
  /**
   * @var string
   */
  public $maxRating;
  /**
   * @var string
   */
  public $merchantDisplayName;
  /**
   * @var string
   */
  public $merchantId;
  /**
   * @var string
   */
  public $merchantLink;
  /**
   * @var string
   */
  public $merchantRatingLink;
  /**
   * @var string
   */
  public $minRating;
  public $rating;
  /**
   * @var string
   */
  public $reviewCountry;
  /**
   * @var string
   */
  public $reviewLanguage;
  /**
   * @var string
   */
  public $reviewTime;
  /**
   * @var string
   */
  public $reviewerId;
  /**
   * @var string
   */
  public $reviewerUsername;
  /**
   * @var string
   */
  public $title;

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
   * @param bool
   */
  public function setIsAnonymous($isAnonymous)
  {
    $this->isAnonymous = $isAnonymous;
  }
  /**
   * @return bool
   */
  public function getIsAnonymous()
  {
    return $this->isAnonymous;
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
  public function setMerchantDisplayName($merchantDisplayName)
  {
    $this->merchantDisplayName = $merchantDisplayName;
  }
  /**
   * @return string
   */
  public function getMerchantDisplayName()
  {
    return $this->merchantDisplayName;
  }
  /**
   * @param string
   */
  public function setMerchantId($merchantId)
  {
    $this->merchantId = $merchantId;
  }
  /**
   * @return string
   */
  public function getMerchantId()
  {
    return $this->merchantId;
  }
  /**
   * @param string
   */
  public function setMerchantLink($merchantLink)
  {
    $this->merchantLink = $merchantLink;
  }
  /**
   * @return string
   */
  public function getMerchantLink()
  {
    return $this->merchantLink;
  }
  /**
   * @param string
   */
  public function setMerchantRatingLink($merchantRatingLink)
  {
    $this->merchantRatingLink = $merchantRatingLink;
  }
  /**
   * @return string
   */
  public function getMerchantRatingLink()
  {
    return $this->merchantRatingLink;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MerchantReviewAttributes::class, 'Google_Service_Merchant_MerchantReviewAttributes');
